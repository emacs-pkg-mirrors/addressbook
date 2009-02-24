;;;; vcard-mode.el --- Major mode for vcard editing

;; Copyright (C) 2008, 2009 Jose E. Marchesi

;; Maintainer: Jose E. Marchesi <jemarch@gnu.org>
;; Keyword: contacts, applications

;; $Id: vcard-mode.el,v 1.4 2009/02/24 22:53:12 zeDek Exp $

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; A major mode for vcard editing.

;;; Code:

(eval-when-compile (require 'cl))
(require 'vcard)
(require 'mm-decode)

;;;; Customizable options

(defcustom vcard-mode-display-groups
  '(identification-properties)
  "Groups to expand by default"
  :type 'sexp
  :group 'vcard)

(defcustom vcard-mode-display-images t
  "Display images in the vcard buffer"
  :type 'boolean
  :group 'vcard)

(defcustom vcard-mode-display-groups
  '(identification-properties)
  "Groups to expand by default"
  :type 'sexp
  :group 'vcard)

(defcustom vcard-mode-attribute-indentation
  1
  "Indentation deep for attribute titles"
  :type 'integer
  :group 'vcard)

(defcustom vcard-mode-field-indentation
  2
  "Indentation deep for attribute fields"
  :type 'integer
  :group 'vcard)

;;;; Faces

(defface vcard-mode-properties-group-name
  '((((min-colors 88) (class color) (background light))
     :foreground "red1")
    (((class color) (background light))
     :foreground "red")
    (((min-colors 88) (class color) (background dark))
     :foreground "yellow1")
    (((class color) (background dark))
     :foreground "yellow")
    (t
     :weight bold))
  "Face for properties group titles"
  :group 'vcard)

(defface vcard-mode-attribute-title-name
  '((((min-colors 88) (class color) (background light))
     )
    (((class color) (background light))
     )
    (((min-colors 88) (class color) (background dark))
     )
    (((class color) (background dark))
     )
    (t
     ))
  "Face for attribute titles"
  :group 'vcard)

(defface vcard-mode-title
  '((((min-colors 88) (class color) (background light))
     :underline t
     )
    (((class color) (background light))
     :underline t
     )
    (((min-colors 88) (class color) (background dark))
     :underline t
     )
    (((class color) (background dark))
     :underline t
     )
    (t
     ))
  "Face for contact titles"
  :group 'vcard)

(defface vcard-mode-attribute-type
  '((((min-colors 88) (class color) (background light))
     :foreground "blue")
    (((class color) (background light))
     :foreground "blue")
    (((min-colors 88) (class color) (background dark))
     :foreground "blue")
    (((class color) (background dark))
     )
    (t
     ))
  "Face for attribute types"
  :group 'vcard)

(defface vcard-mode-attribute-value
  '((((min-colors 88) (class color) (background light))
     :foreground "grey")
    (((class color) (background light))
     :foreground "grey")
    (((min-colors 88) (class color) (background dark))
     :foreground "grey")
    (((class color) (background dark))
     :weight bold)
    (t
     :weight bold))
  "Face for attribute values"
  :group 'vcard)

;;;; Variables

(defvar vcard-mode-properties-nodisplay
  '("sound" "agent" "version" "label" "mailer" "uid"))

(defvar vcard-mode-data nil
  "vCard object.")

(defvar vcard-mode-image-types
  '(("gif" nil)
    ("cgm" nil)
    ("wmf" nil)
    ("bmp" nil)
    ("met" nil)
    ("pbm" pbm)
    ("dib" nil)
    ("pict" nil)
    ("tiff" nil)
    ("pdf" nil)
    ("ps" postscript)
    ("jpeg" jpeg)
    ("mpeg" nil)
    ("mpeg2" nil)
    ("avi" nil)
    ("qtime" nil))
  "Association between vCard image types and emacs image types")

(defvar vcard-mode-properties
  '((identification-properties
     "Identification"
     (("fn" "Formatted Name" nil)
      ("n" "Name" ?n
       (("Surname" ?s)
        ("First name" ?f)
        ("AKA" ?a)
        ("Name prefix" ?p)
        ("Name suffix" ?x)))
      ("photo" "Photograph" ?p nil
       ("type" (("jpeg" "jpeg" ?j)
                ("gif" "gif" ?g)
                ("cgm" "cgm" ?c)
                ("wmf" "wmf" ?w)
                ("bmp" "bmp" ?b)
                ("met" "met" ?m)
                ("pbm" "pbm" ?p)
                ("dib" "dib" ?d)
                ("pict" "pict" ?i)
                ("tiff" "tiff" ?t)
                ("ps" "ps" ?s)
                ("pdf" "pdf" ?f)
                ("mpeg" "mpeg" ?e)
                ("mpeg2" "mpeg2" ?2)
                ("avi" "avi" ?v)
                ("qtime" "qtime" ?q))
        nil t))
      ("bday" "Birthdate" ?b nil))
     ?i)
    (delivering-addressing-properties
     "Delivering Addressing"
     (("adr" "Delivery Address" ?a
       (("Post office address" ?p)
        ("Extended address" ?e)
        ("Street" ?s)
        ("Locality" ?l)
        ("Region" ?r)
        ("Postal code" ?o)
        ("Country" ?c))
       ("type" (("home" "home" ?h)
                ("dom" "domestic" ?d)
                ("intl" "international" ?i)
                ("postal" "postal" ?p)
                ("parcel" "parcel" ?a)
                ("work" "work" ?w))
        t nil))
      ("label" "Delivery Label" ?l nil
       ("type" (("home" "home" ?h)
                ("dom" "domestic" ?d)
                ("intl" "international" ?i)
                ("postal" "postal" ?p)
                ("parcel" "parcel" ?a)
                ("work" "work" ?w))
        t nil)))
     ?d)
    (telecommunications-addressing-properties
     "Telecommunications Addressing"
     (("tel" "Telephone Number" ?t nil
       ("type" (("cell" "cellular" ?c)
                ("home" "home" ?h)
                ("pref" "preferred" ?f)
                ("work" "work" ?w)
                ("voice" "voice" ?v)
                ("fax" "facsimile" ?x)
                ("msg" "messaging service" ?m)
                ("pager" "pager" ?g)
                ("bbs" "bbs" ?b)
                ("modem" "modem" ?o)
                ("car" "car-phone" ?r)
                ("isdn" "isdn" ?i)
                ("video" "video-phone" ?d)
                ("pcs" "pcs" ?p))
        t nil))
      ("email" "Electronic Mail" ?e nil
       ("type" (("internet" "smtp" ?s)
                ("aol" "America On-Line" ?l)
                ("applelink" "AppleLink" ?a)
                ("attmail" "AT&T" ?t)
                ("cis" "cis" ?c)
                ("cworld" "CWorld" ?w)
                ("ibmmail" "IBM mail" ?b)
                ("mcimail" "MCI mail" ?m)
                ("powershare" "powershare" ?p)
                ("prodigy" "prodigy" ?r)
                ("tlx" "telex" ?e)
                ("x400" "X.400" ?x))
        nil t))
      ("mailer" "Mailer" nil nil))
     ?t)
    (geographical-properties
     "Geographical"
     (("tz" "Time Zone" ?z nil)
      ("geo" "Geographic Position" ?g nil))
     ?g)
    (organizational-properties
     "Organizational"
     (("title" "Title" ?i nil)
      ("role" "Business Category" ?r nil)
      ("logo" "Business Logotype" ?w nil
       ("type" (("jpeg" "jpeg" ?j)
                ("gif" "gif" ?g)
                ("cgm" "cgm" ?c)
                ("wmf" "wmf" ?w)
                ("bmp" "bmp" ?b)
                ("met" "met" ?m)
                ("pbm" "pbm" ?p)
                ("dib" "dib" ?d)
                ("pict" "pict" ?i)
                ("tiff" "tiff" ?t)
                ("ps" "ps" ?s)
                ("pdf" "pdf" ?f)
                ("mpeg" "mpeg" ?e)
                ("mpeg2" "mpeg2" ?2)
                ("avi" "avi" ?v)
                ("qtime" "qtime" ?q))
        nil t))
      ("agent" "Agent" nil nil)
      ("org" "Organization" ?o
       (("Name" ?n)
        ("Unit" ?u)
        ("Additional units" ?a))))
     ?o)
    (explanatory-properties
     "Explanatory"
     (("note" "Comment" ?m nil)
      ("rev" "Last Revision" nil nil)
      ("sound" "Sound" ?d nil
       ("type" (("wave" "wave" ?w)
                ("pcm" "pcm" ?p)
                ("aiff" "aiff" ?a))
        nil t))
      ("url" "URL" ?u nil)
      ("uid" "Unique Identifier" nil nil)
      ("version" "Version of vCard" nil nil))
     ?e)
    (security-properties
     "Security"
     (("key" "Public Key" ?k nil
       ("type" (("pgp" "pgp" ?g)
                ("x509" "x509" ?x))
        nil t)))
     ?s))
  "vCard specification standard properties")

;; RFC2426 says we must have at least these attrs
(defvar vcard-mode-required-attrs '("version" "fn" "n")
  "List of required attributes")

(defvar vcard-mode-general-params
  '(("url" "value") ("content-id" "value"))
  "General vCard parameters")

;;;; Display functions

(defun vcard-mode-display-card (vcard-data)
  "Display VCARD-DATA in the current point of the current buffer"
  (save-excursion
    (let ((card vcard-data))
      (progn
        (insert "\n\n")
        ;; Reset displayed groups list
        (setq vcard-mode-displayed-groups nil)
        ;; Display groups
        (mapcar #'vcard-mode-display-group vcard-mode-properties)
        ;; Hide all groups not present in vcard-mode-display-groups
        (dolist (group vcard-mode-displayed-groups nil)
          (if (not (member group vcard-mode-display-groups))
              (vcard-mode-hide-show-group group nil)))))))

(defun vcard-mode-display-group (group)
  (if (vcard-mode-group-has-properties-p group)
      (let ((group-region-begin (make-marker))
            (group-region-end nil))
        (set-marker group-region-begin (point))
        (vcard-mode-display-properties group)
        (insert "\n")
        (setq group-region-end (point))
        (put-text-property (marker-position group-region-begin)
                           group-region-end
                           'group-region (car group)))))

(defun vcard-mode-display-properties (group)
  "Display the GROUP properties from the current card"
  (let* ((card vcard-mode-data)
         (group-name (vcard-mode-get-group-name group))
         (group-props (vcard-mode-get-group-props group))
         (num-attributes (vcard-get-num-attributes card))
         (i 0))
    ;; Mark this group as displayed
    (add-to-list 'vcard-mode-displayed-groups (vcard-mode-get-group-symbol group))
    (insert (propertize group-name 'face 'vcard-mode-properties-group-name
                        'group (vcard-mode-get-group-symbol group)))
    (insert "\n\n")
    (dolist (property group-props)
      (dotimes (i num-attributes)
        (let ((attr (vcard-get-attribute card i)))
          (if (and (equal (vcard-attr-get-name attr) (vcard-mode-get-prop-name property))
                   (not (vcard-mode-attribute-nodisplay attr vcard-mode-properties-nodisplay))
                   (vcard-mode-property-in-group-p attr group-props))
              (vcard-mode-display-attribute i)))))))

(defun vcard-mode-attribute-nodisplay (attr nodisplay-attrs)
  (let ((attr-name (vcard-attr-get-name attr)))
    (if nodisplay-attrs
      (or (equal (car nodisplay-attrs) attr-name)
          (vcard-mode-attribute-nodisplay attr (cdr nodisplay-attrs))))))

(defun vcard-mode-display-attribute (attr-index)
  "Display the ATTR-INDEXth attribute"
  (let* ((card vcard-mode-data)
         (attr (vcard-get-attribute card attr-index))
         (attr-name (vcard-attr-get-name attr)))
    (cond
     ((equal attr-name "fn")
      t)
     ((equal attr-name "n")
      (vcard-mode-display-attribute-n attr-index))
     ((or (equal attr-name "photo")
          (equal attr-name "logo"))
      (vcard-mode-display-attribute-photo-logo attr-index))
     (t
      (vcard-mode-display-attribute-regular attr-index)))))

(defun vcard-mode-display-attribute-n (attr-index)
  (let* ((card vcard-mode-data)
         (attr (vcard-get-attribute card attr-index))
         (attr-value (vcard-attr-get-values attr))
         (surname (nth 0 attr-value))
         (name (nth 1 attr-value))
         (additional-names (nth 2 attr-value))
         (name-prefix (nth 3 attr-value))
         (name-suffix (nth 4 attr-value)))
    (vcard-mode-display-attribute-regular attr-index)
    ;; Insert name on the first line
    (save-excursion
      (goto-char (point-min))
      (if (get-text-property (point) 'title)
          (vcard-mode-erase-tagged-region 'title))
      (insert (propertize
               (vcard-mode-get-card-fn t)
               'face 'vcard-mode-title
               'title t)))))

(defun vcard-mode-display-attribute-photo-logo (attr-index)
  "Display photo from ATTR-INDEX.
Only display it if not already displayed and/or image type is
supported and if `display-images-p' is non nil.

ATTR-INDEX can represent eith an inlined data or an offline url.
When ressource is of type URL, we use url package to get the image data."
  (let* ((card vcard-mode-data)
         (attr (vcard-get-attribute card attr-index))
         (attr-value (car (vcard-attr-get-values attr)))
         (photo-type (car (vcard-attr-get-parameter attr "type")))
         (photo-value (car (vcard-attr-get-parameter attr "value")))
         (image-type nil)
         (image-data nil))
    (vcard-mode-display-attribute-regular attr-index)
    ;; Insert photo in buffer
    ;; Determine emacs image type
    (setq image-type
          (cadr (assoc photo-type vcard-mode-image-types)))

    ;; Display the image or a link
    (when (and vcard-mode-display-images
               (display-images-p)
               image-type
               (image-type-available-p image-type)
               (not (vcard-mode-photo-displayed-p)))

      ;; Get image data
      (let ((image-data
	     (if (equal photo-value "url")
		 (save-excursion
		   (require 'url)

		   (let ((image-buffer (url-retrieve-synchronously attr-value)))
		     (if image-buffer
			 (unwind-protect
			     (with-current-buffer image-buffer
			       ;; FIXME: could be more robust
			       (goto-char (point-min))
			       (re-search-forward "^\r?$" nil 1)
			       (forward-line)
			       (delete-region (point-min) (point))

			       (setq image-data (string-as-unibyte (buffer-string))))
			   (kill-buffer image-buffer)))))
	       attr-value)))

	;; Display the image
	(save-excursion
	  (goto-char (point-min))
	  (goto-char (line-end-position))
	  (insert "\n\n")
	  (insert-image (create-image image-data image-type t)
			(propertize "[photo]"
				    'identification-photo t
				    'attr-index attr-index
				    'attr-subindex nil)))))))

(defun vcard-mode-display-attribute-regular (attr-index)
  (let* ((card vcard-mode-data)
         (attr (vcard-get-attribute card attr-index))
         (attr-name (vcard-attr-get-name attr))
         (attr-value (vcard-attr-get-values attr))
         (property (vcard-mode-get-property attr-name))
         (prop-title (vcard-mode-get-prop-title property))
         (prop-fields (vcard-mode-get-prop-fields property))
         (attr-type (car (vcard-attr-get-parameter attr "type")))
         (attr-region-begin nil)
         (attr-region-end nil))
    (setq attr-region-begin (point))
    (if prop-fields
        (progn
          ;; Insert attribute fields instead of name
          (insert (make-string vcard-mode-attribute-indentation ?\ ))
          (insert (propertize prop-title
                              'face 'vcard-mode-attribute-title-name
                              'attr-compound-title t
                              'attr-index attr-index))
          (vcard-mode-display-attribute-type attr-index)
          (insert ":")
          (insert "\n")
          (dotimes (i (length prop-fields))
            (let ((value (nth i attr-value)))
              (if (and value
                       (not (equal value "")))
                  (progn
                    (insert (make-string
                             (+ vcard-mode-field-indentation
                                vcard-mode-attribute-indentation)
                             ?\ ))
                    (insert (propertize (nth i prop-fields)
                                        'face 'vcard-mode-attribute-title-name
                                        'attr-index attr-index
                                        'attr-subindex i))
                    (insert ":")
                    (insert " ")
                    (insert (propertize (nth i attr-value)
                                        'face 'vcard-mode-attribute-value
                                        'attr-index attr-index
                                        'attr-subindex i))
                    (insert "\n"))))))
      ;; Insert attribute title
      (insert " ")
      (insert (propertize prop-title
                          'face 'vcard-mode-attribute-title-name
                          'attr-index attr-index
                          'attr-subindex nil))
      (vcard-mode-display-attribute-type attr-index)
      (insert ":")
      (insert " ")
      ;; Insert attribute value
      (insert (propertize (car attr-value)
                          'face 'vcard-mode-attribute-value
                          'attr-index attr-index
                          'attr-subindex nil))
      (insert "\n"))

    (setq attr-region-end (point))
    (put-text-property attr-region-begin attr-region-end
                       'attr-region attr-index)))

(defun vcard-mode-display-attribute-type (attr-index)
  (let* ((card vcard-mode-data)
         (attr (vcard-get-attribute card attr-index))
         (attr-name (vcard-attr-get-name attr))
         (property (vcard-mode-get-property attr-name))
         (type-param (vcard-mode-get-prop-parameter property "type"))
         (attr-type-params (vcard-attr-get-parameter attr "type"))
         attr-type
         prop-type-param
         printable-type-list)
    (dolist (attr-type attr-type-params)
      (setq printable-type-list
            (cons (nth 1 (assoc attr-type type-param)) printable-type-list)))
    (setq printable-type-list (reverse printable-type-list))
    (when attr-type-params
      (insert " ")
      (insert "(")
      (insert (propertize
               (vcard-mode-list-to-csv printable-type-list)
               'face 'vcard-mode-attribute-type))
      (insert ")"))))

(defun vcard-mode-group-hidden-p (group)
  (save-excursion
    (let ((group-exist (vcard-mode-goto-group group)))
      (and group-exist
           (get-text-property group-exist 'invisible)))))

(defun vcard-mode-hide-show-group (group show-p)
  "Hide GROUP attributes from the screen"
  (save-excursion
    (let ((group-exist (vcard-mode-goto-group group))
          (group-real-begin-pos nil)
          (group-end-pos nil)
          (group-begin-pos nil))
      (when group-exist
        (setq group-real-begin-pos (next-single-property-change (point) 'group))
        (goto-char group-real-begin-pos)
        (setq group-end-pos (next-single-property-change (point) 'group))
        (if (not group-end-pos)
            (setq group-end-pos (point-max)))
        (if show-p
            (progn
              (remove-text-properties group-real-begin-pos
                                      (- group-end-pos 1)
                                      '(invisible nil)))
          (put-text-property group-real-begin-pos
                             (- group-end-pos 1)
                             'invisible t))))))

(defun vcard-mode-get-current-group ()
  "Return the group affecting current buffer point, or nil"
  (let ((prop-change-pos (previous-single-property-change
                          (point) 'group)))
    (when prop-change-pos
      (save-excursion
        (goto-char (- prop-change-pos 1))
        (get-text-property (point) 'group)))))

(defun vcard-mode-get-current-attr-index ()
  "Return the attribute index of the attribute displayed in the current line"
  (vcard-mode-get-text-property-line 'attr-index))

(defun vcard-mode-get-current-attr-subindex ()
  "Return the attribute subindex of the attribute displayed in the current line"
  (vcard-mode-get-text-property-line 'attr-subindex))

(defun vcard-mode-get-current-attr-compound-title ()
  (vcard-mode-get-text-property-line 'attr-compound-title))

(defun vcard-mode-goto-group (group)
  "Leave the point at the beginning of GROUP"
  (let ((group-begin-pos nil)
        (found nil)
        (group-exist t))
    ;; Search for the first non-nil 'group
    ;; property change with 'group == GROUP
    (goto-char (point-min))
    (while (not found)
      (setq group-begin-pos (next-single-property-change (point) 'group))
      (if group-begin-pos
          (progn
            (goto-char group-begin-pos)
            (if (eq (get-text-property (point) 'group) group)
                (setq found t)))
        (setq found t)
        (setq group-exist nil)))
    group-exist))

(defun vcard-mode-redisplay-card ()
  "Redisplay current card"
  (erase-buffer)
  (vcard-mode-display-card vcard-mode-data))

(defun vcard-mode-redisplay-group (group)
  "Redisplay GROUP in the screen"
  (save-excursion
    (let ((group-exist (vcard-mode-goto-group group)))
      (when group-exist
        ;; Remove old group contents
        (vcard-mode-erase-group-region)
        ;; Display the group
        (vcard-mode-display-group (vcard-mode-get-group group))))))

(defun vcard-mode-erase-group-region ()
  "Erase the region used by the group in point"
  (vcard-mode-erase-tagged-region 'group-region))

(defun vcard-mode-erase-attr-region ()
  "Erase the region used by the attribute in point"
  (vcard-mode-erase-tagged-region 'attr-region))

(defun vcard-mode-redisplay-attr-at-point ()
  "Redisplay the attribute at point"
  (let* ((column-backup (current-column))
         (line-backup (line-number-at-pos (point)))
         (group-symbol (vcard-mode-get-current-group))
         (attr-index (vcard-mode-get-current-attr-index)))
    (if (and group-symbol attr-index)
      (let* ((card vcard-mode-data)
             (group (vcard-mode-get-group group-symbol))
             (group-attrs (vcard-mode-get-group-props group))
             group-aregion-begin group-region-end)
        (vcard-mode-erase-attr-region)
        (setq group-region-begin (point))
        (vcard-mode-display-attribute attr-index)
        (setq group-region-end (point))
        (put-text-property group-region-begin
                           group-region-end
                           'group-region group-symbol)
        (goto-line line-backup)
        (goto-char (+ (line-beginning-position) column-backup))))))

(defun vcard-mode-in-display-p (group-symbol)
  (vcard-mode-goto-group group-symbol))

(defun vcard-mode-photo-displayed-p ()
  (next-single-property-change (point-min) 'identification-photo))

(defun vcard-mode-erase-tagged-region (tag)
  "Erase the region tagged with the same TAG value"
  (let ((begin-pos (previous-single-property-change (point) tag))
        (end-pos (next-single-property-change (point) tag)))
    (if (equal (point) (point-min))
        (setq begin-pos (point-min))
      (if (not (equal (get-text-property (point) tag)
                      (get-text-property (- (point) 1) tag)))
          (setq begin-pos (point))))
    (if (equal (point) (point-max))
        (setq end-pos (point-max))
      (if (not (equal (get-text-property (point) tag)
                      (get-text-property (+ (point) 1) tag)))
          (setq end-pos (+ point 1))))
    (cond ((and begin-pos end-pos)
           (delete-region begin-pos end-pos))
          ((and begin-pos (not end-pos))
           (delete-region begin-pos (point-max)))
          ((and (not begin-pos) end-pos)
           (delete-region (point-min) end-pos)))))

;;;; Properties Management

(defun vcard-mode-get-group (group-symbol)
  "Return the sexp containing information for GROUP"
  (assoc group-symbol vcard-mode-properties))

(defun vcard-mode-get-group-symbol (group)
  (nth 0 group))

(defun vcard-mode-get-group-name (group)
  (nth 1 group))

(defun vcard-mode-get-group-props (group)
  (nth 2 group))

(defun vcard-mode-get-group-letter (group)
  (nth 3 group))

(defun vcard-mode-group-has-properties-p (group)
  (let ((group-attrs (vcard-mode-get-group-props group))
        (result nil))
    (dolist (attr vcard-mode-data)
      (if (and (vcard-mode-property-in-group-p attr group-attrs)
               (not (member (vcard-attr-get-name attr) vcard-mode-properties-nodisplay)))
          (setq result t)))
    result))

(defun vcard-mode-get-group-prop (props prop-name)
  (assoc prop-name props))

(defun vcard-mode-get-prop-name (property)
  (nth 0 property))

(defun vcard-mode-get-prop-title (property)
  (nth 1 property))

(defun vcard-mode-get-prop-letter (property)
  (nth 2 property))

(defun vcard-mode-get-prop-fields-list (property)
  (nth 3 property))

(defun vcard-mode-get-prop-fields (property)
  (let ((fields (vcard-mode-get-prop-fields-list property))
        (result nil) field)
    (dolist (field fields)
      (setq result (cons (car field) result)))
    (reverse result)))

(defun vcard-mode-get-prop-field-name (field)
  (nth 0 field))

(defun vcard-mode-get-prop-field-letter (field)
  (nth 1 field))

(defun vcard-mode-get-prop-index (prop-fields field-name)
  (let ((index 0) result)
    (dotimes (index (length prop-fields) result)
      (if (equal (nth index prop-fields) field-name)
          (setq result index)))))

(defun vcard-mode-get-prop-field-description (fields field-name)
  (cadr (assoc field-name fields)))

(defun vcard-mode-get-prop-parameters (prop)
  (nthcdr 4 prop))

(defun vcard-mode-get-prop-parameter (prop param-name)
  (let ((prop-parameters (vcard-mode-get-prop-parameters prop)))
    (cadr (assoc param-name prop-parameters))))

(defun vcard-mode-prop-parameter-allow-duplicates (prop param-name)
  (let ((prop-parameters (vcard-mode-get-prop-parameters prop)))
    (nth 2 (assoc param-name prop-parameters))))

(defun vcard-mode-prop-parameter-is-mandatory (prop param-name)
  (let ((prop-parameters (vcard-mode-get-prop-parameters prop)))
    (nth 3 (assoc param-name prop-parameters))))

(defun vcard-mode-property-in-group-p (attr group-props)
  (let ((attr-name (vcard-attr-get-name attr)))
    (when (assoc attr-name group-props)
      t)))

(defun vcard-mode-get-property (attr-name)
  (let (group result)
    (dolist (group vcard-mode-properties)
      (let* ((group-props (vcard-mode-get-group-props group))
             (group-prop (vcard-mode-get-group-prop group-props attr-name)))
        (if group-prop
            (setq result group-prop))))
    result))

(defun vcard-mode-get-prop-default-type (prop-name)
  (let* ((property (vcard-mode-get-property prop-name))
         (prop-type (vcard-mode-get-prop-parameter property "type")))
    (when prop-type
      (car (car prop-type)))))

(defun vcard-mode-value-empty-p (values)
  "Return t if VALUES is empty"
  (when (listp values)
    (if (cdr values)
        (when (equal (car values) "")
          (vcard-mode-value-empty-p (cdr values)))
      (equal (car values) ""))))

(defun vcard-mode-number-of-values (values)
  (if (listp values)
      (let (value
            (nov 0))
        (dolist (value values nov)
          (if (not (equal value ""))
              (setq nov (+ nov 1)))))
    1))

(defun vcard-mode-get-card-fn (&optional with-aka card-number)
  (let* ((card vcard-mode-data)
         (name-attr (vcard-get-named-attribute card "n"))
         (name-attr-values (vcard-attr-get-values name-attr))
         (name-surname (nth 0 name-attr-values))
         (name-surname-p (and name-surname
                              (not (equal name-surname ""))))
         (name-first-name (nth 1 name-attr-values))
         (name-first-name-p (and name-first-name
                                 (not (equal name-first-name ""))))
         (name-aka (nth 2 name-attr-values))
         (name-aka-p (and name-aka
                          (not (equal name-aka ""))))
         (name-prefix (nth 3 name-attr-values))
         (name-prefix-p (and name-prefix
                             (not (equal name-prefix ""))))
         (name-suffix (nth 4 name-attr-values))
         (name-suffix-p (and name-suffix
                             (not (equal name-suffix ""))))
         (result ""))
    (when name-prefix-p
      (setq result (concat result name-prefix)))
    (when name-first-name-p
      (setq result (concat result
                           (when name-prefix-p " ")
                           name-first-name)))
    (when name-surname-p
      (setq result (concat result
                           (when (or name-prefix-p
                                     name-first-name-p) " ")
                           name-surname)))
    (when name-suffix-p
      (setq result (concat result
                           (when (or name-prefix-p
                                     name-first-name-p
                                     name-surname-p) " ")
                           name-suffix)))
    (when (and with-aka name-aka-p)
      (setq result (concat result
                           (when (or name-prefix-p
                                     name-first-name-p
                                     name-surname-p
                                     name-suffix-p) " ")
                           "(" name-aka ")")))
    result))

(defun vcard-mode-delete-attr (attr-index attr-subindex)
  (let* ((card vcard-mode-data)
         (attr (vcard-get-attribute card attr-index))
         (attr-value (vcard-attr-get-values attr)))
    (if attr-subindex
        (progn
          ;; Delete the field from the values
          (setcar (nthcdr attr-subindex attr-value) "")
          (vcard-attr-set-values attr attr-value)
          (if (vcard-mode-value-empty-p attr-value)
              (setq vcard-mode-data (vcard-delete-indexed-attribute card attr-index))))
      ;; Delete the attribute
      (setq vcard-mode-data (vcard-delete-indexed-attribute card attr-index)))))

(defun vcard-mode-build-custom-property-group ()
  "Return an empty custom property group"
  (list 'custom-properties
        "Custom Properties"
        nil
        ?c))

(defun vcard-mode-set-custom-properties (props-data)
"This function accepts a list of the form:

  (PROP1 PROP2 ... PROPN)

where each property PROP is defined with the following structure:

  (\"property-name\" \"Property displayed name\"
   ?character-identifying-the-group-for-fast-selection
   (FIELD1 FIELD2 ... FIELDN)
   (\"type\"
      ((\"type1\" \"type1 displayed name\" ?fast-selection-char1)
       (\"type2\" \"type 2 displayed name\" ?fast-selection-char2)
       ...)
    allow-several-types-p at-least-one-type-mandatory-p))

where each field FIELD is defined with the following structure:

  (\"Field name\" ?character-identifying-the-field-for-fast-selection)"
  (let (custom-group
        prop)
    (dolist (prop props-data)
      (setcar prop (concat "x-emacs-" (car prop))))
    (when (not (vcard-mode-get-group 'custom-properties))
      (setq vcard-mode-properties
            (append vcard-mode-properties (list (vcard-mode-build-custom-property-group)))))
    (setq custom-group (vcard-mode-get-group 'custom-properties))
    (setcar (nthcdr 2 custom-group) props-data)))



;;;; Commands

(defun vcard-mode-add-attribute-type ()
  "Add a new type to the attribute under point"
  (interactive)
  (let ((buffer-read-only nil)
        (point-backup (point))
        (group-symbol (vcard-mode-get-current-group))
        (attr-index (vcard-mode-get-current-attr-index))
        (attr-subindex (vcard-mode-get-current-attr-subindex)))
    (if (and attr-index
             (not attr-subindex))
        (let* ((card vcard-mode-data)
               (attr (vcard-get-attribute card attr-index))
               (attr-name (vcard-attr-get-name attr))
               (property (vcard-mode-get-property attr-name))
               (prop-types (vcard-mode-get-prop-parameter property "type")))
          (if prop-types
              (let ((new-type (vcard-mode-select-non-existing-type attr))
                    type result)
                (dolist (type prop-types)
                  (if (equal (cadr type)
                             new-type)
                      (setq result (car type))))
                (when result
                  (if (vcard-mode-prop-parameter-allow-duplicates property "type")
                      ;; Add the new type
                      (vcard-attr-add-property attr "type" result)
                    ;; Replace current type
                    (vcard-attr-set-property attr "type" result))
                  ;; Redisplay attribute
                  (vcard-mode-redisplay-attr-at-point)
                  ;; Addressbook modified
                  (add-to-list 'addrbook-modified-cards vcard-mode-data))))))
    (goto-char point-backup)))

(defun vcard-mode-remove-attribute-type ()
  "Remove a type from the attribute under point"
  (interactive)
  (let ((buffer-read-only nil)
        (point-backup (point))
        (group-symbol (vcard-mode-get-current-group))
        (attr-index (vcard-mode-get-current-attr-index))
        (attr-subindex (vcard-mode-get-current-attr-subindex)))
    (if (and attr-index
             (not attr-subindex))
        (let* ((card vcard-mode-data)
               (attr (vcard-get-attribute card attr-index))
               (attr-name (vcard-attr-get-name attr))
               (property (vcard-mode-get-property attr-name))
               (prop-types (vcard-mode-get-prop-parameter property "type")))
          (if prop-types
              (if (and (equal (length (vcard-attr-get-parameter attr "type")) 1)
                       (vcard-mode-prop-parameter-is-mandatory property "type"))
                  (message "This attribute should have a type")
                (let ((new-type (vcard-mode-select-existing-type attr))
                      type result)
                  (dolist (type prop-types)
                    (if (equal (cadr type)
                               new-type)
                        (setq result (car type))))
                  (when result
                    ;; Add the new type
                    (vcard-attr-remove-property attr "type" result)
                    ;; Redisplay attribute
                    (vcard-mode-redisplay-attr-at-point)
                    ;; Addressbook modified
                    (add-to-list 'addrbook-modified-cards vcard-mode-data)))))))
    (goto-char point-backup)))

(defun vcard-mode-delete-attribute ()
  "Delete the attribute under point"
  (interactive)
  (let ((buffer-read-only nil)
        (point-backup (point))
        (group-symbol (vcard-mode-get-current-group))
        (attr-index (vcard-mode-get-current-attr-index))
        (attr-subindex (vcard-mode-get-current-attr-subindex)))
  (if (and group-symbol attr-index)
      (let* ((group (vcard-mode-get-group group-symbol))
             (group-attrs (vcard-mode-get-group-props group))
             (card vcard-mode-data)
             (attr (vcard-get-attribute card attr-index))
             (attr-name (vcard-attr-get-name attr))
             (attr-value (vcard-attr-get-values attr))
             (group-attr (vcard-mode-get-group-prop group-attrs attr-name))
             (attr-title (vcard-mode-get-prop-title group-attr))
             (attr-fields (vcard-mode-get-prop-fields-list group-attr))
             (attr-field (when attr-subindex (nth attr-subindex attr-fields)))
             (attr-field-name (when attr-field (vcard-mode-get-prop-field-name attr-field)))
             (prompt (concat "Are you sure you want to delete "
                             (if attr-subindex
                                 (concat "field " attr-field-name)
                               (concat "attribute " attr-title))
                             "? "))
             elt)
        (if (yes-or-no-p prompt)
            (if (and (member attr-name vcard-mode-required-attrs)
                     (or (vcard-mode-get-current-attr-compound-title)
                         (equal (vcard-mode-number-of-values attr-value) 1)))
                (error "Trying to delete a required attribute")
              (vcard-mode-delete-attr attr-index attr-subindex)
              (if (not (equal attr-name "photo"))
                  (vcard-mode-redisplay-group group-symbol)
                (vcard-mode-redisplay-card))))))
  (goto-char point-backup)
  (add-to-list 'addrbook-modified-cards vcard-mode-data)))

(defun vcard-mode-add-attribute ()
  "Add a new attribute to the current card"
  (interactive)
  (let* (buffer-read-only
         (backup-point (point))
         group-symbol
         group group-attrs
         (i 0)
         (current-card vcard-mode-data))
    ;; Get group
    (setq group-symbol (or (vcard-mode-get-current-group)
                           (vcard-mode-select-group)))
    (setq group (vcard-mode-get-group group-symbol))
    (setq group-attrs (vcard-mode-get-group-props group))
    (if group-symbol
        (let (attr-index attr-subindex property-index)
          ;; Get property
          (setq attr-index (vcard-mode-get-current-attr-index))
          (setq attr-subindex (vcard-mode-get-current-attr-subindex))
          (if (and attr-index attr-subindex)
              (let ((attr (vcard-get-attribute current-card attr-index)))
                (setq property-name (vcard-attr-get-name attr)))
            (setq property-name (vcard-mode-select-property group-symbol)))
          (if property-name
              (let* ((property (vcard-mode-get-group-prop group-attrs property-name))
                     (property-title (vcard-mode-get-prop-title property))
                     (property-fields (vcard-mode-get-prop-fields property))
                     field-index (property-value "") prompt
                     (continue t))
                ;; Get field
                (when property-fields
                  (setq field-index (vcard-mode-select-field group-symbol property-name))
                  (setq continue field-index))
                (when continue
                  ;; Ask for a new value for the property or field
                  (setq prompt (concat
                                property-title
                                (if field-index
                                    (concat " ("
                                            (nth field-index property-fields)
                                            ")"))
                                ": "))
                  ;; Read value from minibuffer
                  (while (equal property-value "")
                    (setq property-value
                          (read-from-minibuffer prompt)))
                  (if (and attr-index property-fields)
                      (let* ((attr (vcard-get-attribute current-card attr-index))
                             (attr-values (vcard-attr-get-values attr))
                             (attr-value (nthcdr field-index attr-values)))
                        ;; Add a field to a specific attribute
                        (if attr-value
                            (setcar attr-value property-value)
                          (setq attr-values (reverse attr-values))
                          ;; Add enough empty values and then the new value
                          (dotimes (i (- field-index (length attr-values)))
                            (setq attr-values (cons "" attr-values)))
                          (setq attr-values (cons property-value attr-values))
                          (setq attr-values (reverse attr-values))
                          (vcard-attr-set-values attr attr-values)))
                    ;; Create a new attribute
                    (let* ((new-attr-type (vcard-mode-get-prop-default-type property-name))
                           (new-attr-name property-name)
                           (new-attr-values property-value)
                           new-attr)
                      (setq new-attr (list (list new-attr-name)
                                           new-attr-values))
                      (vcard-attr-set-property new-attr "type" new-attr-type)
                      (if (equal new-attr-name "photo")
                          (vcard-attr-set-property new-attr "value" "url"))
                      (setq current-card (vcard-add-attribute current-card new-attr))))
                  (setq vcard-mode-data current-card)
                  (if (vcard-mode-in-display-p group-symbol)
                      (progn
                        ;; Redisplay the group with new contents
                        (vcard-mode-redisplay-group group-symbol)
                        ;; Hide the group if it was hidden
                        (if (vcard-mode-group-hidden-p group)
                            (vcard-mode-hide-show-group group nil)))
                    ;; Redisplay the entire card
                    (vcard-mode-redisplay-card))
                  ;; This card has been modified
                  (add-to-list 'addrbook-modified-cards addrbook-current-card))))))
    (goto-char backup-point)))

(defun vcard-mode-edit-attribute ()
  (interactive)
  "Edit the value of the attribute located in the current line"
  (let ((buffer-read-only nil)
        (group-symbol (vcard-mode-get-current-group))
        (attr-index (vcard-mode-get-current-attr-index))
        (attr-subindex (vcard-mode-get-current-attr-subindex))
        (attr-compound-title-p (vcard-mode-get-current-attr-compound-title)))
    (if (and group-symbol attr-index (not attr-compound-title-p))
        (let* ((group (vcard-mode-get-group group-symbol))
               (group-attrs (vcard-mode-get-group-props group))
               (card vcard-mode-data)
               (attr (vcard-get-attribute card attr-index))
               (attr-name (vcard-attr-get-name attr))
               (attr-value (vcard-attr-get-values attr))
               (group-attr (vcard-mode-get-group-prop group-attrs attr-name))
               (attr-fields (vcard-mode-get-prop-fields group-attr))
               (attr-real-value (if attr-subindex
                                    (nth attr-subindex attr-value)
                                  (car attr-value)))
               (attr-real-name (if attr-subindex
                                   (nth attr-subindex attr-fields)
                                 (vcard-mode-get-prop-title group-attr)))
               (new-value nil))
          ;; Ask for a new value for the attribute
          (setq new-value
                (read-from-minibuffer (concat attr-real-name ": ")
                                      attr-real-value))
          ;; Set the new value into the cards list
          ;; attr-fields[attr-subindex], attr-value, attr
          (let ((new-values nil))
            (if attr-subindex
                (setcar (nthcdr attr-subindex attr-value) new-value)
              (setq new-values (list new-value))
              (vcard-attr-set-values attr new-values)))
          ;; FIXME: update Last Revision field
          ;; Mark the current card as modified
          (add-to-list 'addrbook-modified-cards addrbook-current-card)
          ;; Redisplay attribute
          ;; FIXME: use vcard-mode-redisplay-attr-at-point
          (let ((column-backup (current-column))
                (line-backup (line-number-at-pos (point))))
            (vcard-mode-erase-attr-region)
            (vcard-mode-display-attribute attr-index)
            (goto-line line-backup)
            (goto-char (+ (line-beginning-position) column-backup)))))))

(defun vcard-mode-goto-next-group ()
  "Leave the point at the beginning of the next group"
  (let ((next-point))
    (setq next-point (next-single-property-change (point) 'group))
    (when next-point
      (if (get-text-property next-point 'group)
          (goto-char next-point)
        (goto-char next-point)
        (setq next-point (next-single-property-change (point) 'group))
        (when next-point
          (goto-char next-point))))))

(defun vcard-mode-toggle-hide-show-group ()
  "When staying on a parameters group title, toggle visibility of the group"
  (interactive)
  (let ((buffer-read-only nil)
        (group (get-text-property (point) 'group))
        (group-content-pos nil))
    (when group
      ;; Search for visibility properties in group contents
      (setq group-content-pos (next-single-property-change (point) 'group))
      (if (get-text-property group-content-pos 'invisible)
          (vcard-mode-hide-show-group group t)
        (vcard-mode-hide-show-group group nil)))))

(defun vcard-mode-hide-all-groups ()
  "Hide all displayed groups"
  (interactive)
  (let (buffer-read-only)
    (dolist (group vcard-mode-displayed-groups nil)
      (vcard-mode-hide-show-group group nil))))

(defun vcard-mode-show-all-groups ()
  "Show all displayed groups"
  (interactive)
  (let (buffer-read-only)
    (dolist (group vcard-mode-displayed-groups nil)
      (vcard-mode-hide-show-group group t))))

(defun vcard-mode-cycle-groups ()
  "Cycle to next group"
  (interactive)
  (let ((next-group-pos (vcard-mode-goto-next-group)))
    (when (not next-group-pos)
      (goto-char (point-min))
      (vcard-mode-goto-next-group))))

;;;;; FIXME: this should be implemented in `before-save-hook'
;;;;; and `auto-save-hook'
(defun vcard-clean-vcard ()
  "Hook to save the vcard contents to disk"
  (if (not (buffer-modified-p))
      (message "(No changes need to be saved)")
    ;; Save the card data
    (vcard-insert vcard-mode-data)
    (message (concat "Wrote " (buffer-file-name)))))

;;;; Utility functions

(defun vcard-mode-get-text-property-line (prop)
  "Return the value of text property PROP in the nearest position on current line
that has PROP defined as a text property"
  (let ((current-point (get-text-property (point) prop))
        (next-point-with-prop (next-single-property-change
                               (point) prop nil (line-end-position)))
        (previous-point-with-prop (previous-single-property-change
                                   (point) prop nil (line-beginning-position))))
    (or current-point
        (if next-point-with-prop
            (get-text-property next-point-with-prop prop)
          (get-text-property previous-point-with-prop prop)))))

(defun vcard-mode-list-to-csv (list)
  (let ((result "")
        i)
    (dotimes (i (length list))
      (setq result (concat result (nth i list)))
      (if (not (equal i (- (length list) 1)))
          (setq result (concat result ","))))
    result))

(defun vcard-mode-make-params-explicit ()
  "Make unambiguous anonymous params explicit.

It uses `vcard-mode-general-params' and the type parameter for each property
defined in `vcard-mode-properties'"
  (let ((i 0))
    (let ((card vcard-mode-data))
      (dotimes (i (vcard-get-num-attributes card))
        (let* ((attr (vcard-get-attribute card i))
               (attr-name (vcard-attr-get-name attr))
               (attr-props (cdr (vcard-attr-get-proplist attr)))
               (property (vcard-mode-get-property attr-name))
               param
               (j 0))
          (dotimes (j (length attr-props))
            (let* ((param (nth j attr-props))
                   (param-name (if (and param
                                        (listp param))
                                   (car param)
                                 nil))
                   (param-value (if (and param
                                         (listp param))
                                    (cdr param)
                                  param)))
              ;; Search the param name in general-value
              (if (not param-name)
                  (let* ((general-param (assoc param-value vcard-mode-general-params))
                         (general-param-name (if general-param (cadr general-param)))
                         (prop-types (vcard-mode-get-prop-parameter property "type")))
                    (if general-param-name
                        (setq param-name general-param-name)
                      (if (and prop-types
                               (assoc param-value prop-types))
                          (setq param-name "type")))
                    (if param-name
                        (setcar (nthcdr j attr-props) (cons param-name param-value))))))))))))

(defun vcard-mode-fast-selection (names prompt)
  "Fast group tag selection with single keys.

NAMES is an association list of the form:

    ((\"NAME1\" char1) ...)

Each character should identify only one name."
  ;; Adapted from `org-fast-tag-selection' in org.el by Carsten Dominic
  ;; Thanks Carsten! ;P
  (let* ((maxlen (apply 'max (mapcar (lambda (name)
                                       (string-width (car name))) names)))
         (buf (current-buffer))
         (fwidth (+ maxlen 3 1 3))
         (ncol (/ (- (window-width) 4) fwidth))
         name count result char i key-list)
    (save-window-excursion
      (set-buffer (get-buffer-create " *vcard-mode Groups*"))
      (delete-other-windows)
      (split-window-vertically)
      (switch-to-buffer-other-window (get-buffer-create " *vcard-mode Groups*"))
      (erase-buffer)
      (insert prompt ":")
      (insert "\n\n")
      (setq count 0)
      (while (setq name (pop names))
        (setq key-list (cons (cadr name) key-list))
        (insert "[" (cadr name) "] "
                (car name)
                (make-string (- fwidth 4 (length (car name))) ?\ ))
        (when (= (setq count (+ count 1)) ncol)
          (insert "\n")
          (setq count 0)))
      (goto-char (point-min))
      (if (fboundp 'fit-window-to-buffer)
          (fit-window-to-buffer))
      (catch 'exit
        (while t
          (message "[a-z0-9...]: Select entry   [RET]: Exit")
          (setq char (let ((inhibit-quit t)) (read-char-exclusive)))
          (cond
           ((= char ?\r)
            (setq result nil)
            (throw 'exit t))
           ((member char key-list)
            (setq result char)
            (throw 'exit t)))))
      result)))

(defun vcard-mode-select-type (attr-name)
  (let* ((property (vcard-mode-get-property attr-name))
         (prop-types (vcard-mode-get-prop-parameter property "type")))
    (let (type-names type letter result)
      (dolist (type prop-types)
        (setq type-names
              (cons (cdr type) type-names)))
      (setq type-names (reverse type-names))
      (setq letter (vcard-mode-fast-selection type-names "Select attribute type"))
      (if letter
          (dolist (type type-names)
            (if (equal letter
                       (cadr type))
                (setq result (car type)))))
      result)))

(defun vcard-mode-select-non-existing-type (attr)
  (let* ((attr-name (vcard-attr-get-name attr))
         (property (vcard-mode-get-property attr-name))
         (prop-types (vcard-mode-get-prop-parameter property "type"))
         (attr-types (vcard-attr-get-parameter attr "type")))
    (let (type-names type letter result)
      (dolist (type prop-types)
        (if (not (member (car type) attr-types))
            (setq type-names
                  (cons (cdr type) type-names))))
      (setq type-names (reverse type-names))
      (setq letter (vcard-mode-fast-selection type-names "Select attribute type"))
      (if letter
          (dolist (type type-names)
            (if (equal letter
                       (cadr type))
                (setq result (car type)))))
      result)))

(defun vcard-mode-select-existing-type (attr)
  (let* ((attr-name (vcard-attr-get-name attr))
         (property (vcard-mode-get-property attr-name))
         (prop-types (vcard-mode-get-prop-parameter property "type"))
         (attr-types (vcard-attr-get-parameter attr "type")))
    (let (type-names type letter result)
      (dolist (type prop-types)
        (if (member (car type) attr-types)
            (setq type-names
                  (cons (cdr type) type-names))))
      (setq type-names (reverse type-names))
      (setq letter (vcard-mode-fast-selection type-names "Select attribute type"))
      (if letter
          (dolist (type type-names)
            (if (equal letter
                       (cadr type))
                (setq result (car type)))))
      result)))

(defun vcard-mode-select-group ()
  "Select a group interactively and return its symbol"
  (let (names group group-elt letter result)
    ;; Build the names list
    (dolist (group-elt vcard-mode-properties)
      (setq names
            (cons (list (vcard-mode-get-group-name group-elt)
                        (vcard-mode-get-group-letter group-elt))
                  names)))
    (setq names (reverse names))
    ;; Call the fast menu function to get the desired group
    (setq letter (vcard-mode-fast-selection names "Select group"))
    (dolist (group-elt vcard-mode-properties)
      (if (and (vcard-mode-get-group-letter group-elt)
               (equal letter (vcard-mode-get-group-letter group-elt)))
          (setq result (vcard-mode-get-group-symbol group-elt))))
    result))

(defun vcard-mode-select-property (group-symbol)
  "Select a property interactively from GROUP and return its name"
  (let* ((group (vcard-mode-get-group group-symbol))
         (group-props (vcard-mode-get-group-props group))
         names attr attr-elt letter result)
    ;; Build the names list
    (dolist (prop group-props)
      (if (and (not (member (vcard-mode-get-prop-name prop) vcard-mode-required-attrs))
               (vcard-mode-get-prop-letter prop))
          (setq names
                (cons (list (vcard-mode-get-prop-title prop)
                            (vcard-mode-get-prop-letter prop))
                      names))))
    (setq names (reverse names))
    ;; Call the fast menu function to get the desired group
    (setq letter (vcard-mode-fast-selection names "Select property"))
    (dolist (prop group-props)
      (if (and (vcard-mode-get-prop-letter prop)
               (equal letter (vcard-mode-get-prop-letter prop)))
          (setq result (vcard-mode-get-prop-name prop))))
    result))

(defun vcard-mode-select-field (group-symbol prop-name)
  "Select a field interactively from PROP-NAME"
  (let* ((group (vcard-mode-get-group group-symbol))
         (group-props (vcard-mode-get-group-props group))
         (property (assoc prop-name group-props))
         (prop-fields (vcard-mode-get-prop-fields-list property))
         letter field result i)
    (setq letter (vcard-mode-fast-selection prop-fields "Select property field"))
    (dotimes (i (length prop-fields))
      (setq field (nth i prop-fields))
      (if (equal letter (vcard-mode-get-prop-field-letter field))
          (setq result i)))
    result))

;;;; Modeline management

(defun vcard-mode-set-mode-line (card-number total-cards)
  "Update the modeline of the current buffer"
  ;; FIXME: this is ugly
  (when vcard-mode-mode-line-string
    (setq mode-line-buffer-identification
	  (list 24
              vcard-mode-mode-line-string
              ": "
              (list 10
                    (format "%d/%d" card-number total-cards))))))

;;;; Major mode

(defun vcard-mode ()
  "A major mode for vcard editing

Commands:
\\{vcard-mode-map}"
  (interactive)
  (unless (eq major-mode 'vcard-mode)
    (let ((modified (buffer-modified-p))
          (inhibit-read-only t)
          (original-point (- (point) (point-min)))
          (original-text-overlay (make-overlay (point-min) (point-max)
                                               nil t nil)))
      (and (eobp) (not (bobp))
	   (setq original-point (1- original-point)))
      (kill-all-local-variables)
      (make-local-variable 'vcard-mode-data)
      (make-local-variable 'vcard-mode-displayed-groups)
      (setq vcard-mode-map (make-keymap))
      (define-key vcard-mode-map "e" 'vcard-mode-edit-attribute)
      (define-key vcard-mode-map (kbd "SPC") 'vcard-mode-toggle-hide-show-group)
      (define-key vcard-mode-map (kbd "TAB") 'vcard-mode-cycle-groups)
      (define-key vcard-mode-map "d" 'vcard-mode-delete-attribute)
      (define-key vcard-mode-map "a" 'vcard-mode-add-attribute)
      (define-key vcard-mode-map "t" 'vcard-mode-add-attribute-type)
      (define-key vcard-mode-map "r" 'vcard-mode-remove-attribute-type)
      (define-key vcard-mode-map "m" 'vcard-mode-send-email)
      (define-key vcard-mode-map "H" 'vcard-mode-hide-all-groups)
      (define-key vcard-mode-map "S" 'vcard-mode-show-all-groups)
      (use-local-map vcard-mode-map)
      (setq mode-name "VCard")
      (setq major-mode 'vcard-mode)
      ;; Parse vcard data in the current buffer
      (setq vcard-mode-data (car (vcard-parse-string
                                  (buffer-substring (point-min) (point-max)))))
      ;; Hide original vcard data
      (overlay-put original-text-overlay 'invisible t)
      ;; Insert visible contents
      (goto-char (point-min))
      (vcard-mode-display-card vcard-mode-data)
      ;; Restore buffer properties
      (restore-buffer-modified-p modified)
      (setq buffer-read-only t))))

(provide 'vcard-mode)

;;; vcard-mode.el ends here
