;;; vcard.el --- vcard parsing routines

;; Copyright (C) 1997, 1999, 2000 Noah S. Friedman
;; Copyright (C) 2007, 2008, 2009 Jose E. Marchesi

;; Author: Noah Friedman <friedman@splode.com>
;; Maintainer: jemarch@gnu.org
;; Keywords: vcard, mail, news
;; Created: 1997-09-27

;; $Id: vcard.el,v 1.9 2009/12/23 15:31:40 jemarch Exp $

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Unformatted vcards are just plain ugly.  But if you live in the MIME
;; world, they are a better way of exchanging contact information than
;; freeform signatures since the former can be automatically parsed and
;; stored in a searchable index.
;;
;; This library of routines provides the back end necessary for parsing
;; vcards so that they can eventually go into an address book like BBDB
;; (although this library does not implement that itself).

;; This library does not interface directly with any mail user agents.  For
;; an example of bindings for the VM MUA, see vm-vcard.el available from
;;
;;    http://www.splode.com/~friedman/software/emacs-lisp/index.html#mail
;;
;; Updates to vcard.el should be available there too.

;; The vcard 2.1 format is defined by the versit consortium.
;; See http://www.imc.org/pdi/vcard-21.ps
;;
;; RFC 2426 defines the vcard 3.0 format.
;; See ftp://ftp.rfc-editor.org/in-notes/rfc2426.txt

;; A parsed vcard is a list of attributes of the form
;;
;;     (proplist value1 value2 ...)
;;
;; Where proplist is a list of property names and parameters, e.g.
;;
;;     (property1 (property2 . parameter2) ...)
;;
;; Each property has an associated implicit or explicit parameter value
;; (not to be confused with attribute values; in general this API uses
;; `parameter' to refer to property values and `value' to refer to attribute
;; values to avoid confusion).  If a property has no explicit parameter value,
;; the parameter value is considered to be `t'.  Any property which does not
;; exist for an attribute is considered to have a nil parameter.

;; TODO:
;;   * Finish supporting the 3.0 extensions.
;;     Currently, only the 2.1 standard is supported.
;;   * Handle nested vcards and grouped attributes?
;;     (I've never actually seen one of these in use.)
;;   * Handle multibyte charsets.

;;; Code:

(defgroup vcard nil
  "Support for the vCard electronic business card format."
  :group 'vcard
  :group 'mail
  :group 'news)

;;;###autoload
(defcustom vcard-standard-filters
  '(vcard-filter-html
    vcard-filter-adr-newlines
    vcard-filter-tel-normalize
    vcard-filter-textprop-cr)
  "*Standard list of filters to apply to parsed vcard data.
These filters are applied sequentially to vcard attributes when
the function `vcard-standard-filter' is supplied as the second argument to
`vcard-parse'."
  :type 'hook
  :group 'vcard)


;;; No user-settable options below.

;; XEmacs 21 ints and chars are disjoint types.
;; For all else, treat them as the same.
(defalias 'vcard-char-to-int
  (if (fboundp 'char-to-int) 'char-to-int 'identity))

;; This is just the version number for this package; it does not refer to
;; the vcard format specification.  Currently, this package does not yet
;; support the full vcard 3.0 specification.
;;
;; Whenever any part of the API defined in this package change in a way
;; that is not backward-compatible, the major version number here should be
;; incremented.  Backward-compatible additions to the API should be
;; indicated by increasing the minor version number.
(defconst vcard-api-version "3.0")

;; The vcard standards allow specifying the encoding for an attribute using
;; these values as immediate property names, rather than parameters of the
;; `encoding' property.  If these are encountered while parsing, associate
;; them as parameters of the `encoding' property in the returned structure.
(defvar vcard-encoding-tags
  '("quoted-printable" "base64" "8bit" "7bit" "b"))

;; The vcard parser will auto-decode these encodings when they are
;; encountered.  These methods are invoked via vcard-parse-region-value.
(defvar vcard-region-decoder-methods
  '(("quoted-printable" . vcard-region-decode-quoted-printable)
    ("base64"           . vcard-region-decode-base64)
    ("b"                . vcard-region-decode-base64)))

(defvar vcard-string-encoder-methods
  '(("quoted-printable" . quoted-printable-encode-string)
    ("base64"           . base64-encode-string)
    ("b"                . base64-encode-string)))

(defvar vcard-regexp-begin-vcard "^begin:[ \t]*vcard[ \t]*\n"
  "Regexp to match the begin of a vcard")
(defvar vcard-regexp-end-vcard "^end[ \t]*:[ \t]*vcard[ \t]*$"
  "Regexp to match the end of a vcard")


;;; Parsing routines

;;;###autoload
(defun vcard-parse-string (raw &optional filter)
  "Parse RAW vcard data as a string, and return an alist representing data.

If the optional function FILTER is specified, apply that filter to each
attribute.  If no filter is specified, `vcard-standard-filter' is used.

Filters should accept two arguments: the property list and the value list.
Modifying in place the property or value list will affect the resulting
attribute in the vcard alist.

Vcard data is normally in the form

    begin:                        vcard
    prop1a:                       value1a
    prop2a;prop2b;prop2c=param2c: value2a
    prop3a;prop3b:                value3a;value3b;value3c
    end:                          vcard

\(Whitespace around the `:' separating properties and values is optional.\)
If supplied to this function an alist of the form

    \(\(\(\"prop1a\"\) \"value1a\"\)
     \(\(\"prop2a\" \"prop2b\" \(\"prop2c\" . \"param2c\"\)\) \"value2a\"\)
     \(\(\"prop3a\" \"prop3b\"\) \"value3a\" \"value3b\" \"value3c\"\)\)

would be returned."
  (let ((vcard nil)
        (buf (generate-new-buffer " *vcard parser work*")))
    (unwind-protect
        (save-excursion
          (set-buffer buf)
          ;; Make sure last line is newline-terminated.
          ;; An extra trailing newline is harmless.
          (insert raw "\n")
          (setq vcard (vcard-parse-region (point-min) (point-max) filter)))
      (kill-buffer buf))
    vcard))

;;;###autoload
(defun vcard-parse-region (beg end &optional filter)
  "Parse the raw vcard data in region, and return an alist representing data.
This function is just like `vcard-parse-string' except that it operates on
a region of the current buffer rather than taking a string as an argument.

Note: this function modifies the buffer!"
  (or filter
      (setq filter 'vcard-standard-filter))
  (let ((case-fold-search t)
        (vcard-list-data nil)
        (pos (make-marker))
        (newpos (make-marker))
        properties value)
    (save-restriction
      (narrow-to-region beg end)
      (save-match-data
        ;; Unfold folded lines and delete naked carriage returns
        (goto-char (point-min))
        (while (re-search-forward "\r$\\|\n[ \t]" nil t)
          (goto-char (match-beginning 0))
          (delete-char 1))
        (goto-char (point-min))
        (while (re-search-forward vcard-regexp-begin-vcard nil t)
          (let ((vcard-data nil))
            (set-marker pos (point))
            (while (and (not (looking-at vcard-regexp-end-vcard))
                        (re-search-forward ":[ \t]*" nil t))
              (set-marker newpos (match-end 0))
              (setq properties
                    (vcard-parse-region-properties pos (match-beginning 0)))
              (set-marker pos (marker-position newpos))
              (re-search-forward "[ \t]*\n")
              (set-marker newpos (match-end 0))
              (setq value
                    (vcard-parse-region-value properties pos (match-beginning 0)))
              (set-marker pos (marker-position newpos))
              (goto-char pos)
              (funcall filter properties value)
              (setq vcard-data (cons (cons properties value) vcard-data)))
            (setq vcard-list-data (cons (nreverse vcard-data) vcard-list-data))))))
    (nreverse vcard-list-data)))

(defun vcard-parse-region-properties (beg end)
  (downcase-region beg end)
  (let* ((proplist (vcard-split-string (buffer-substring beg end) ";"))
         (props proplist)
         split)
    (save-match-data
      (while props
        (cond ((string-match "=" (car props))
               (setq split (vcard-split-string (car props) "=" 2))
               (setcar props (cons (car split) (car (cdr split)))))
              ((member (car props) vcard-encoding-tags)
               (setcar props (cons "encoding" (car props)))))
        (setq props (cdr props))))
    proplist))

(defun vcard-parse-region-value (proplist beg end)
  (let* ((encoding (vcard-get-property proplist "encoding"))
         (decoder (cdr (assoc encoding vcard-region-decoder-methods)))
         result pos match-beg match-end)
    (save-restriction
      (narrow-to-region beg end)
      (cond (decoder
             ;; Each `;'-separated field needs to be decoded and saved
             ;; separately; if the entire region were decoded at once, we
             ;; would not be able to distinguish between the original `;'
             ;; chars and those which were encoded in order to quote them
             ;; against being treated as field separators.
             (goto-char beg)
             (setq pos (set-marker (make-marker) (point)))
             (setq match-beg (make-marker))
             (setq match-end (make-marker))
             (save-match-data
               (while (< pos (point-max))
                 (cond ((search-forward ";" nil t)
                        (set-marker match-beg (match-beginning 0))
                        (set-marker match-end (match-end 0)))
                       (t
                        (set-marker match-beg (point-max))
                        (set-marker match-end (point-max))))
                 (funcall decoder pos match-beg)
                 (setq result (cons (buffer-substring pos match-beg) result))
                 (set-marker pos (marker-position match-end))))
             (setq result (nreverse result)))
            (t
             (setq result (vcard-split-string (buffer-string) ";")))))
    (goto-char (point-max))
    result))

(defun vcard-parse-file (filename)
  "Parse FILENAME as a vCard object and return it.
Parsing occurs with `vcard-parse-region'."
  (let ((result nil))
    (with-temp-buffer
      (insert-file filename)
      (setq result (vcard-parse-region (point-min) (point-max))))
    (if result
	result
      (error "FILENAME parsing error !"))))


;;; Functions for retrieving property or value information from parsed
;;; vcard attributes.

(defun vcard-values (vcard have-props &optional non-props limit)
  "Return the values in VCARD.
This function is like `vcard-ref' and takes the same arguments, but return
only the values, not the associated property lists."
  (mapcar 'cdr (vcard-ref vcard have-props non-props limit)))

(defun vcard-ref (vcard have-props &optional non-props limit)
  "Return the attributes in VCARD with HAVE-PROPS properties.
Optional arg NON-PROPS is a list of properties which candidate attributes
must not have.
Optional arg LIMIT means return no more than that many attributes.

The attributes in VCARD which have all properties specified by HAVE-PROPS
but not having any specified by NON-PROPS are returned.  The first element
of each attribute is the actual property list; the remaining elements are
the values.

If a specific property has an associated parameter \(e.g. an encoding\),
use the syntax \(\"property\" . \"parameter\"\) to specify it.  If property
parameter is not important or it has no specific parameter, just specify
the property name as a string."
  (let ((attrs vcard)
        (result nil)
        (count 0))
    (while (and attrs (or (null limit) (< count limit)))
      (and (vcard-proplist-all-properties (car (car attrs)) have-props)
           (not (vcard-proplist-any-properties (car (car attrs)) non-props))
           (setq result (cons (car attrs) result)
                 count (1+ count)))
      (setq attrs (cdr attrs)))
    (nreverse result)))

(defun vcard-proplist-all-properties (proplist props)
  "Returns nil unless PROPLIST contains all properties specified in PROPS."
  (let ((result t))
    (while (and result props)
      (or (vcard-get-property proplist (car props))
          (setq result nil))
      (setq props (cdr props)))
    result))

(defun vcard-proplist-any-properties (proplist props)
  "Returns `t' if PROPLIST contains any of the properties specified in PROPS."
  (let ((result nil))
    (while (and (not result) props)
      (and (vcard-get-property proplist (car props))
           (setq result t))
      (setq props (cdr props)))
    result))

(defun vcard-get-property (proplist property)
  "Return the value from PROPLIST of PROPERTY.
PROPLIST is a vcard attribute property list, which is normally the first
element of each attribute entry in a vcard."
  (or (and (member property proplist) t)
      (cdr (assoc property proplist))))

(defun vcard-set-property (proplist property value)
  "In PROPLIST, set PROPERTY to VALUE.
PROPLIST is a vcard attribute property list.
If VALUE is nil, PROPERTY is deleted."
  (let (elt)
    (cond ((null value)
           (vcard-delete-property proplist property))
          ((setq elt (member property proplist))
           (and value (not (eq value t))
                (setcar elt (cons property value))))
          ((setq elt (assoc property proplist))
           (cond ((eq value t)
                  (setq elt (memq elt proplist))
                  (setcar elt property))
                 (t
                  (setcdr elt value))))
          ((eq value t)
           (nconc proplist (cons property nil)))
          (t
           (nconc proplist (cons (cons property value) nil))))))

(defun vcard-delete-property (proplist property)
  "Delete from PROPLIST the specified property PROPERTY.
This will not succeed in deleting the first member of the proplist, but
that element should never be deleted since it is the primary key."
  (let (elt)
    (cond ((setq elt (member property proplist))
           (delq (car elt) proplist))
          ((setq elt (assoc property proplist))
           (delq (car (memq elt proplist)) proplist)))))

(defun vcard-get-attribute (vcard index)
  "Return the INDEXth attribute from VCARD.
Return nil if INDEX is out of bounds"
  (cond ((or (< index 0) (>= index (length vcard)))
         nil)
        (t
         (nth index vcard))))

(defun vcard-get-named-attribute (vcard attr-name)
  (let (attr result)
    (dolist (attr vcard)
      (if (equal attr-name (vcard-attr-get-name attr))
          (setq result attr)))
    result))

(defun vcard-set-attribute (vcard index attribute)
  "Set ATTRIBUTE as the new INDEXth attribute in VCARD in a destructive way.
Do nothing if INDEX is out of bounds."
  (cond ((or (< index 0) (>= index (length vcard)))
         nil)
        (t
         (setcar (nthcdr index vcard) attribute))))


(defun vcard-get-indexed-property (proplist index)
  "Return the INDEXth property from PROPLIST.
Return nil if INDEX if out of bounds."
  (cond ((or (< index 0) (>= index (length proplist)))
         nil)
        (t
         (nth index proplist))))

(defun vcard-set-indexed-property (proplist index value)
  "Set VALUE as the new INDEXth parameter value in VCARD in a destructive way.
Do nothing if INDEX is out of bounds."
  (cond ((or (< index 0) (>= index (length proplist)))
         nil)
        (t
         (let ((prop (nth index proplist)))
           (if (listp prop)
               (setcar (nthcdr index proplist) (cons (car prop) value))
             (setcar (nthcdr index proplist) value))))))

(defun vcard-delete-indexed-property (proplist index)
  "Return a list with the INDEXth property from PROPLIST deleted.
Do nothing if INDEX is out of bounds."
  (cond ((or (< index 0) (>= index (length proplist)))
         nil)
        (t
         (let ((prop (nth index proplist)))
           (setq proplist (delq prop proplist))))))

(defun vcard-attr-get-values (attr)
  "Return a list with the values of ATTR"
  (cdr attr))

(defun vcard-attr-set-values (attr values)
  "Set VALUES (a list of values) as the ATTR's values, in a destructive way."
  (setcdr attr values))

(defun vcard-attr-get-proplist (attr)
  "Return the properties list of ATTR"
  (car attr))

(defun vcard-attr-set-proplist (attr proplist)
  "Set ATTR's property list to PROPLIST in a destructive way"
  (setcar attr proplist))

(defun vcard-delete-attribute (vcard proplist)
  "Return a VCARD with a properties list equal to PROPLIST deleted
in a destructive way."
  (let ((elt (assoc proplist vcard)))
    (if elt
        (delq elt vcard))))

(defun vcard-delete-indexed-attribute (vcard index)
  "Return a VCARD with the INDEXth attribute deleted"
  (delete-if (lambda (elt) t)
             vcard
             :start index
             :end (+ index 1)))

(defun vcard-add-attribute (vcard attr)
  "Return a vcard composed of attributes of VCARD plus ATTR"
  (if (not (member attr vcard))
      (setq vcard (reverse (cons attr vcard)))
    vcard))

(defun vcard-get-num-attributes (vcard)
  "Return the number of attributes contained in VCARD"
  (length vcard))

(defun vcard-attr-get-name (attr)
  "Return the first property of ATTR"
  (car (vcard-attr-get-proplist attr)))

(defun vcard-attr-get-parameter (attr property)
  "Return the parameter value associated with PROPERTY in ATTR.
Return nil if PROPERTY is not a property of ATTR."
  (let* ((proplist (vcard-attr-get-proplist attr))
         (result nil))
    (dolist (prop proplist nil)
      (if (listp prop)
          (if (equal property (car prop))
              (setq result (cons (cdr prop) result)))
        (if (equal property prop)
            (setq result (cons t result)))))
    result))

(defun vcard-attr-set-property (attr property value)
  (let ((proplist (vcard-attr-get-proplist attr)))
    (vcard-set-property proplist property value)))

(defun vcard-attr-add-property (attr property value)
  "Add a new property to ATTR with PROPERTY and VALUE"
  (let ((proplist (vcard-attr-get-proplist attr)))
    (setcdr proplist (cons (cons property value) (cdr proplist)))
    (vcard-attr-set-proplist attr proplist)))

(defun vcard-attr-remove-property (attr property value)
  "Remove the PROPERTY-VALUE in ATTR"
  (let ((proplist (vcard-attr-get-proplist attr))
        prop i prop-index-to-delete)
    (dotimes (i (length proplist))
      (setq prop (nth i proplist))
      (if (or (and (listp prop)
                   (equal (car prop) property)
                   (equal (cdr prop) value))
              (and (not (listp prop))
                   (equal value t)
                   (equal prop property)))
          (setq prop-index-to-delete i)))
    (if prop-index-to-delete
        (setq proplist (vcard-delete-indexed-property proplist
                                                      prop-index-to-delete)))
    (vcard-attr-set-proplist attr proplist)))


;;; Vcard writing routines

(defun vcard-insert (vcard)
  "Insert the textual representation of VCARD in the current buffer.
Leave the point after the last inserted character.
VCARD is a parsed vCard."
  ;; XXX: Take care about encodings!!!
  (insert "begin: vcard")
  (insert "\n")
  (dolist (attr vcard)
    (let ((proplist (vcard-attr-get-proplist attr))
          (values (vcard-attr-get-values attr))
          encoding encoder)
      (dotimes (i (length proplist))
        (let ((prop (nth i proplist)))
          (if (listp prop)
              (insert (concat (car prop)
                              "="
                              (cdr prop)))
            (insert prop)))
        (if (not (equal i (- (length proplist) 1)))
            (insert ";")))
      (insert ":")
      (insert " ")
      (setq encoding (vcard-get-property proplist "encoding"))
      (setq encoder (cdr (assoc encoding vcard-string-encoder-methods)))
      (dotimes (i (length values))
        (let ((value (nth i values)))
          (when encoder
            (setq value (funcall encoder value)))
          (insert value)
          (if (not (equal i (- (length values) 1)))
              (insert ";"))))
      (insert "\n")))
  (insert "end: vcard"))

(defun vcard-to-string (vcard)
  "Return a string with VCARD's textual representation"
  (save-excursion
    (with-temp-buffer
      (vcard-insert vcard)
      (buffer-substring (point-min) (point-max)))))




;;; Vcard data filters.
;;;
;;; Filters receive both the property list and value list and may modify
;;; either in-place.  The return value from the filters are ignored.
;;;
;;; These filters can be used for purposes such as removing HTML tags or
;;; normalizing phone numbers into a standard form.

(defun vcard-standard-filter (proplist values)
  "Apply filters in `vcard-standard-filters' to attributes."
  (vcard-filter-apply-filter-list vcard-standard-filters proplist values))

;; This function could be used to dispatch other filter lists.
(defun vcard-filter-apply-filter-list (filter-list proplist values)
  (while filter-list
    (funcall (car filter-list) proplist values)
    (setq filter-list (cdr filter-list))))

;; Some lusers put HTML (or even javascript!) in their vcards under the
;; misguided notion that it's a standard feature of vcards just because
;; Netscape supports this feature.  That is wrong; the vcard specification
;; does not define any html content semantics and most MUAs cannot do
;; anything with html text except display them unparsed, which is ugly.
;;
;; Thank Netscape for abusing the standard and damned near rendering it
;; useless for interoperability between MUAs.
;;
;; This filter does a very rudimentary job.
(defun vcard-filter-html (proplist values)
  "Remove HTML tags from attribute values."
  (save-match-data
    (while values
      (while (string-match "<[^<>\n]+>" (car values))
        (setcar values (replace-match "" t t (car values))))
      (setq values (cdr values)))))

(defun vcard-filter-adr-newlines (proplist values)
  "Replace newlines with \"; \" in `adr' values."
  (and (vcard-get-property proplist "adr")
       (save-match-data
         (while values
           (while (string-match "[\r\n]+" (car values))
             (setcar values (replace-match "; " t t (car values))))
           (setq values (cdr values))))))

(defun vcard-filter-tel-normalize (proplist values)
  "Normalize telephone numbers in `tel' values.
Spaces and hyphens are replaced with `.'.
US domestic telephone numbers are replaced with international format."
  (and (vcard-get-property proplist "tel")
       (save-match-data
         (while values
           (while (string-match "[\t._-]+" (car values))
             (setcar values (replace-match " " t t (car values))))
           (and (string-match "^(?\\(\\S-\\S-\\S-\\))? ?\
\\(\\S-\\S-\\S- \\S-\\S-\\S-\\S-\\)"
                              (car values))
                (setcar values
                        (replace-match "+1 \\1 \\2" t nil (car values))))
           (setq values (cdr values))))))

(defun vcard-filter-textprop-cr (proplist values)
  "Strip carriage returns from text values."
  (and (vcard-proplist-any-properties
        proplist '("adr" "email" "fn" "label" "n" "org" "tel" "title" "url"))
       (save-match-data
         (while values
           (while (string-match "\r+" (car values))
             (setcar values (replace-match "" t t (car values))))
           (setq values (cdr values))))))


;;; Decoding methods.

(defmacro vcard-hexstring-to-ascii (s)
  (if (string-lessp emacs-version "20")
      `(format "%c" (car (read-from-string (format "?\\x%s" ,s))))
    `(format "%c" (string-to-number ,s 16))))

(defun vcard-region-decode-quoted-printable (&optional beg end)
  (save-excursion
    (save-restriction
      (save-match-data
        (narrow-to-region (or beg (point-min)) (or end (point-max)))
        (goto-char (point-min))
        (while (re-search-forward "=\n" nil t)
          (delete-region (match-beginning 0) (match-end 0)))
        (goto-char (point-min))
        (while (re-search-forward "=[0-9A-Za-z][0-9A-Za-z]" nil t)
          (let ((s (buffer-substring (1+ (match-beginning 0)) (match-end 0))))
            (replace-match (vcard-hexstring-to-ascii s) t t)))))))

(defun vcard-region-decode-base64 (beg end)
  (goto-char beg)
  (while (re-search-forward " +" end t)
    (replace-match "\n"))
  (when (> (point) end)
    (setq end (point)))
  (base64-decode-region beg end))

(defun vcard-split-string (string &optional separator limit)
  "Split STRING at occurences of SEPARATOR.  Return a list of substrings.
Optional argument SEPARATOR can be any regexp, but anything matching the
 separator will never appear in any of the returned substrings.
 If not specified, SEPARATOR defaults to \"[ \\f\\t\\n\\r\\v]+\".
If optional arg LIMIT is specified, split into no more than that many
 fields \(though it may split into fewer\)."
  (or separator (setq separator "[ \f\t\n\r\v]+"))
  (let ((string-list nil)
        (len (length string))
        (pos 0)
        (splits 0)
        str)
    (save-match-data
      (while (<= pos len)
        (setq splits (1+ splits))
        (cond ((and limit
                    (>= splits limit))
               (setq str (substring string pos))
               (setq pos (1+ len)))
              ((string-match separator string pos)
               (setq str (substring string pos (match-beginning 0)))
               (setq pos (match-end 0)))
              (t
               (setq str (substring string pos))
               (setq pos (1+ len))))
        (setq string-list (cons str string-list))))
    (nreverse string-list)))

(defun vcard-copy-tree (tree)
  "Make a deep copy of nested conses."
  (cond
   ((consp tree)
    (cons (vcard-copy-tree (car tree))
          (vcard-copy-tree (cdr tree))))
   (t tree)))

(defun vcard-flatten (l)
  (if (consp l)
      (apply 'nconc (mapcar 'vcard-flatten l))
    (list l)))

(provide 'vcard)

;;; vcard.el ends here.
