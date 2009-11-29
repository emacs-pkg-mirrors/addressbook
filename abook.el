;;; abook.el --- A simple addressbook

;; Copyright (C) 2007, 2008, 2009 Jose E. Marchesi

;; Maintainer: Jose E. Marchesi
;; Keywords: contacts, applications

;; $Id: abook.el,v 1.19 2009/11/29 15:05:16 jemarch Exp $

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

;; A simple vCard based addressbook for Emacs
;;
;; To see the structure of this file activate the outline minor mode
;; and execute M-xhide-body

;;; Code:

(eval-when-compile (require 'cl))
(require 'vcard)
(require 'mm-decode)

;; * Constants

(defconst abook-version "0.1"
  "Version of the addressbook")

;; * Customization

(defgroup abook nil
  "Addressbook subsytem"
  :group 'applications
  :link '(url-link "http://www.emacswiki.org/cgi-bin/wiki/AddressBook"))

(defgroup abook-hooks nil
  "Addressbook hooks"
  :group 'abook)

(defcustom abook-display-images t
  "Display images in the addressbook"
  :type 'boolean
  :group 'abook)

(defcustom abook-display-groups
  '(identification-properties)
  "Groups to expand by default"
  :type 'sexp
  :group 'abook)

(defcustom abook-force-addressbook-creation
  t
  "Force the creation of the addressbook file if it doesnt exist upon startup"
  :type 'boolean
  :group 'abook)

(defcustom abook-attribute-indentation
  1
  "Indentation deep for attribute titles"
  :type 'integer
  :group 'abook)

(defcustom abook-field-indentation
  2
  "Indentation deep for attribute fields"
  :type 'integer
  :group 'abook)

(defcustom abook-ask-for-search
  nil
  "Whether the addressbook should ask for a search upon `abook' invocation"
  :type 'boolean
  :group 'abook)

(defcustom abook-field-for-sort
  "First name"
  "Field to use when sorting contacts.

It may be \"Surname\", \"First Name\", \"AKA\", \"Name prefix\" or \"Name suffix\"."
  :type 'string
  :group 'abook)

(defcustom abook-use-multiple-frames
  nil
  "If t, open new frames when switching summary<->contact"
  :type 'boolean
  :group 'abook)

(defface abook-summary-card-number
  '((((min-colors 88) (class color) (background light))
     :foreground "red1")
    (((class color) (background light))
     :foreground "red")
    (((min-colors 88) (class color) (background dark))
     :foreground "blue")
    (((class color) (background dark))
     :foreground "blue")
    (t
     :weight bold))
  "Face for summary card numbers"
  :group 'abook)

(defface abook-summary-modified-flag
  '((((min-colors 88) (class color) (background light))
     :foreground "red1")
    (((class color) (background light))
     :foreground "red")
    (((min-colors 88) (class color) (background dark))
     :foreground "red")
    (((class color) (background dark))
     :foreground "red")
    (t
     :weight bold))
  "Face for summary modified flag"
  :group 'abook)

(defface abook-summary-match-flag
  '((((min-colors 88) (class color) (background light))
     :foreground "brown")
    (((class color) (background light))
     :foreground "brown")
    (((min-colors 88) (class color) (background dark))
     :foreground "brown")
    (((class color) (background dark))
     :foreground "brown")
    (t
     :weight bold))
  "Face for summary match flag"
  :group 'abook)

(defface abook-properties-group-name
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
  :group 'abook)

(defface abook-attribute-title-name
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
  :group 'abook)

(defface abook-contact-title
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
  :group 'abook)

(defface abook-attribute-type
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
  :group 'abook)

(defface abook-attribute-value
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
  :group 'abook)

(defface abook-summary-selected-card
  '((((min-colors 88) (class color) (background light))
     :background "grey"
     :foreground "black")
    (((class color) (background light))
     :background "grey"
     :foreground "black")
    (((min-colors 88) (class color) (background dark))
     :background "grey"
     :foreground "black"
     :weight bold)
    (((class color) (background dark))
     :weight bold)
    (t
     :weight bold))
  "Face for selected summary contact line"
  :group 'abook)

;; * Variables

(defvar abook-image-types
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

(defvar abook-cards nil
  "Cards of the current addressbook")

(defvar abook-modified-cards nil
  "Indexes of modified cards in abook-cards")

(defvar abook-current-card nil
  "Number of current card")

(defvar abook-properties
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
                ("pref" "preferred" ?p)
                ("work" "work" ?w)
                ("voice" "voice" ?v)
                ("fax" "facsimile" ?f)
                ("msg" "messaging service" ?m)
                ("pager" "pager" ?g)
                ("bbs" "bbs" ?b)
                ("modem" "modem" ?o)
                ("car" "car-phone" ?r)
                ("isdn" "isdn" ?i)
                ("video" "video-phone" ?d))
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

(defvar abook-required-attrs '("n")
  "List of required attributes")

(defvar abook-general-params
  '(("url" "value") ("content-id" "value"))
  "General vCard parameters")

;; * Properties management functions

;; ** Groups
(defun abook-get-group (group-symbol)
  "Return the sexp containing information for GROUP"
  (assoc group-symbol abook-properties))

(defun abook-get-group-symbol (group)
  (nth 0 group))

(defun abook-get-group-name (group)
  (nth 1 group))

(defun abook-get-group-props (group)
  (nth 2 group))

(defun abook-get-group-letter (group)
  (nth 3 group))

(defun abook-group-has-properties-p (group)
  (let ((group-attrs (abook-get-group-props group))
        (result nil))
    (dolist (attr (abook-get-card abook-current-card))
      (if (and (abook-property-in-group-p attr group-attrs)
               (not (member (vcard-attr-get-name attr) abook-contact-properties-nodisplay)))
          (setq result t)))
    result))

;; ** Properties

(defun abook-get-group-prop (props prop-name)
  (assoc prop-name props))

(defun abook-get-prop-name (property)
  (nth 0 property))

(defun abook-get-prop-title (property)
  (nth 1 property))

(defun abook-get-prop-letter (property)
  (nth 2 property))

(defun abook-get-prop-fields-list (property)
  (nth 3 property))

(defun abook-get-prop-fields (property)
  (let ((fields (abook-get-prop-fields-list property))
        (result nil) field)
    (dolist (field fields)
      (setq result (cons (car field) result)))
    (reverse result)))

(defun abook-get-prop-field-name (field)
  (nth 0 field))

(defun abook-get-prop-field-letter (field)
  (nth 1 field))

(defun abook-get-prop-index (prop-fields field-name)
  (let ((index 0) result)
    (dotimes (index (length prop-fields) result)
      (if (equal (nth index prop-fields) field-name)
          (setq result index)))))

(defun abook-get-prop-field-description (fields field-name)
  (cadr (assoc field-name fields)))

(defun abook-get-prop-parameters (prop)
  (nthcdr 4 prop))

(defun abook-get-prop-parameter (prop param-name)
  (let ((prop-parameters (abook-get-prop-parameters prop)))
    (cadr (assoc param-name prop-parameters))))

(defun abook-prop-parameter-allow-duplicates (prop param-name)
  (let ((prop-parameters (abook-get-prop-parameters prop)))
    (nth 2 (assoc param-name prop-parameters))))

(defun abook-prop-parameter-is-mandatory (prop param-name)
  (let ((prop-parameters (abook-get-prop-parameters prop)))
    (nth 3 (assoc param-name prop-parameters))))

(defun abook-property-in-group-p (attr group-props)
  (let ((attr-name (vcard-attr-get-name attr)))
    (when (assoc attr-name group-props)
      t)))

(defun abook-get-property (attr-name)
  (let (group result)
    (dolist (group abook-properties)
      (let* ((group-props (abook-get-group-props group))
             (group-prop (abook-get-group-prop group-props attr-name)))
        (if group-prop
            (setq result group-prop))))
    result))

(defun abook-get-prop-default-type (prop-name)
  (let* ((property (abook-get-property prop-name))
         (prop-type (abook-get-prop-parameter property "type")))
    (when prop-type
      (car (car prop-type)))))

;; ** Cards

(defun abook-get-card (numcard)
  (nth numcard abook-cards))

(defun abook-set-card (numcard card)
  (if abook-cards
      (cond
       ((and (>= numcard 0) (< numcard (length abook-cards)))
        (setcar (nthcdr numcard abook-cards) card))
       ((>= numcard (length abook-cards))
        (setq abook-cards (append abook-cards (list card)))))
    (setq abook-cards (list card))))

(defun abook-remove-card (numcard)
  (setq abook-cards (delete (abook-get-card numcard)
                               abook-cards)))

(defun abook-value-empty-p (values)
  "Return t if VALUES is empty"
  (when (listp values)
    (if (cdr values)
        (when (equal (car values) "")
          (abook-value-empty-p (cdr values)))
      (equal (car values) ""))))

(defun abook-number-of-values (values)
  (if (listp values)
      (let (value
            (nov 0))
        (dolist (value values nov)
          (if (not (equal value ""))
              (setq nov (+ nov 1)))))
    1))

(defun abook-get-card-fn (&optional with-aka card-number)
  (let* ((card (abook-get-card (if card-number
                                      card-number
                                    abook-current-card)))
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

;; ** Attributes

(defun abook-delete-attr (attr-index attr-subindex)
  (let* ((card (abook-get-card abook-current-card))
         (attr (vcard-get-attribute card attr-index))
         (attr-value (vcard-attr-get-values attr)))
    (if attr-subindex
        (progn
          ;; Delete the field from the values
          (setcar (nthcdr attr-subindex attr-value) "")
          (vcard-attr-set-values attr attr-value)
          (if (abook-value-empty-p attr-value)
              (abook-set-card abook-current-card (vcard-delete-indexed-attribute card attr-index))))
      ;; Delete the attribute
      (abook-set-card abook-current-card (vcard-delete-indexed-attribute card attr-index)))))

(defun abook-build-custom-property-group ()
  "Return an empty custom property group"
  (list 'custom-properties
        "Custom Properties"
        nil
        ?c))

(defun abook-set-custom-properties (props-data)
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
    (when (not (abook-get-group 'custom-properties))
      (setq abook-properties
            (append abook-properties (list (abook-build-custom-property-group)))))
    (setq custom-group (abook-get-group 'custom-properties))
    (setcar (nthcdr 2 custom-group) props-data)))


;; * Addressbook contact editor

;; ** Constants

(defconst abook-contact-buffer-name "*ABook Contact*"
  "Name of the buffer for the addressbook contact editor")

;; ** Variables

(defvar abook-contact-properties-nodisplay
  '("sound" "agent" "version" "uid" "label" "mailer" "uid"))

(defvar abook-contact-mode-map nil
  "Keymap for abook-contact-mode")

(defvar abook-contact-displayed-groups nil
  "List of displayed property groups")

(defvar abook-contact-mode-line-string " ABook Contact"
  "String to display on the mode line when in the addressbook mode.
If `nil', do not show anything.")

;; ** Contact buffer management

(defun abook-create-contact-buffer ()
  "Create a new addressbook buffer to show contact information"
  (setq buffer (get-buffer-create abook-contact-buffer-name))
  (set-buffer buffer)
  (abook-contact-mode))

(defun abook-show-contact ()
  (let ((buffer (get-buffer abook-contact-buffer-name)))
    (if abook-use-multiple-frames
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))

;; ** Display functions

(defun abook-contact-display-card (numcard)
  "Display the NUMCARD card into the addressbook buffer"
  (save-excursion
    (let ((card (abook-get-card numcard)))
      (if card
          (progn
            (erase-buffer)
            (setq abook-current-card numcard)
            (insert "\n\n")
            ;; Reset displayed groups list
            (setq abook-contact-displayed-groups nil)
            ;; Display groups
            (mapcar #'abook-contact-display-group abook-properties)
            ;; Hide all groups not present in abook-display-groups
            (dolist (group abook-contact-displayed-groups nil)
              (if (not (member group abook-display-groups))
                  (abook-contact-hide-show-group group nil)))
            ;; Set mode line contents
            (abook-contact-set-mode-line (+ abook-current-card 1)
                                    (length abook-cards)))))))

(defun abook-contact-display-group (group)
  (if (abook-group-has-properties-p group)
      (let ((group-region-begin (make-marker))
            (group-region-end nil))
        (set-marker group-region-begin (point))
        (abook-contact-display-properties group)
        (insert "\n")
        (setq group-region-end (point))
        (put-text-property (marker-position group-region-begin)
                           group-region-end
                           'group-region (car group)))))

(defun abook-contact-display-properties (group)
  "Display the GROUP properties from the current card"
  (let* ((card (abook-get-card abook-current-card))
         (group-name (abook-get-group-name group))
         (group-props (abook-get-group-props group))
         (num-attributes (vcard-get-num-attributes card))
         (i 0))
    ;; Mark this group as displayed
    (add-to-list 'abook-contact-displayed-groups (abook-get-group-symbol group))
    (insert (propertize group-name 'face 'abook-properties-group-name
                        'group (abook-get-group-symbol group)))
    (insert "\n\n")
    (dolist (property group-props)
      (dotimes (i num-attributes)
        (let ((attr (vcard-get-attribute card i)))
          (if (and (equal (vcard-attr-get-name attr) (abook-get-prop-name property))
                   (not (abook-contact-attribute-nodisplay attr abook-contact-properties-nodisplay))
                   (abook-property-in-group-p attr group-props))
              (abook-contact-display-attribute i)))))))

(defun abook-contact-attribute-nodisplay (attr nodisplay-attrs)
  (let ((attr-name (vcard-attr-get-name attr)))
    (if nodisplay-attrs
      (or (equal (car nodisplay-attrs) attr-name)
          (abook-contact-attribute-nodisplay attr (cdr nodisplay-attrs))))))

(defun abook-contact-display-attribute (attr-index)
  "Display the ATTR-INDEXth attribute"
  (let* ((card (abook-get-card abook-current-card))
         (attr (vcard-get-attribute card attr-index))
         (attr-name (vcard-attr-get-name attr)))
    (cond
     ((equal attr-name "fn")
      t)
     ((equal attr-name "n")
      (abook-contact-display-attribute-n attr-index))
     ((or (equal attr-name "photo")
          (equal attr-name "logo"))
      (abook-contact-display-attribute-photo-logo attr-index))
     (t
      (abook-contact-display-attribute-regular attr-index)))))

(defun abook-contact-display-attribute-n (attr-index)
  (let* ((card (abook-get-card abook-current-card))
         (attr (vcard-get-attribute card attr-index))
         (attr-value (vcard-attr-get-values attr))
         (surname (nth 0 attr-value))
         (name (nth 1 attr-value))
         (additional-names (nth 2 attr-value))
         (name-prefix (nth 3 attr-value))
         (name-suffix (nth 4 attr-value)))
    (abook-contact-display-attribute-regular attr-index)
    ;; Insert name on the first line
    (save-excursion
      (goto-char (point-min))
      (if (get-text-property (point) 'title)
          (abook-erase-tagged-region 'title))
      (insert (propertize
               (abook-get-card-fn t)
               'face 'abook-contact-title
               'title t)))))

(defun abook-contact-display-attribute-photo-logo (attr-index)
  "Display photo from ATTR-INDEX.
Only display it if not already displayed and/or image type is
supported and if `display-images-p' is non nil.

ATTR-INDEX can represent eith an inlined data or an offline url.
When ressource is of type URL, we use url package to get the image data."
  (let* ((card (abook-get-card abook-current-card))
         (attr (vcard-get-attribute card attr-index))
         (attr-value (car (vcard-attr-get-values attr)))
         (photo-type (car (vcard-attr-get-parameter attr "type")))
         (photo-value (car (vcard-attr-get-parameter attr "value")))
         (image-type nil)
         (image-data nil))
    ;; Insert photo in buffer
    ;; Determine emacs image type
    (setq image-type
          (cadr (assoc photo-type abook-image-types)))
    ;; Display the image or a link
    (when (and abook-display-images
               (display-images-p)
               image-type
               (image-type-available-p image-type)
               (not (abook-contact-photo-displayed-p)))
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
	       (string-as-unibyte attr-value))))
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

(defun abook-contact-display-attribute-regular (attr-index)
  (let* ((card (abook-get-card abook-current-card))
         (attr (vcard-get-attribute card attr-index))
         (attr-name (vcard-attr-get-name attr))
         (attr-value (vcard-attr-get-values attr))
         (property (abook-get-property attr-name))
         (prop-title (abook-get-prop-title property))
         (prop-fields (abook-get-prop-fields property))
         (attr-type (car (vcard-attr-get-parameter attr "type")))
         (attr-region-begin nil)
         (attr-region-end nil))
    (setq attr-region-begin (point))
    (if prop-fields
        (progn
          ;; Insert attribute fields instead of name
          (insert (make-string abook-attribute-indentation ?\ ))
          (insert (propertize prop-title
                              'face 'abook-attribute-title-name
                              'attr-compound-title t
                              'attr-index attr-index))
          (abook-contact-display-attribute-type attr-index)
          (insert ":")
          (insert "\n")
          (dotimes (i (length prop-fields))
            (let ((value (nth i attr-value)))
              (if (and value
                       (not (equal value "")))
                  (progn
                    (insert (make-string
                             (+ abook-field-indentation
                                abook-attribute-indentation)
                             ?\ ))
                    (insert (propertize (nth i prop-fields)
                                        'face 'abook-attribute-title-name
                                        'attr-index attr-index
                                        'attr-subindex i))
                    (insert ":")
                    (insert " ")
                    (insert (propertize (nth i attr-value)
                                        'face 'abook-attribute-value
                                        'attr-index attr-index
                                        'attr-subindex i))
                    (insert "\n"))))))
      ;; Insert attribute title
      (insert " ")
      (insert (propertize prop-title
                          'face 'abook-attribute-title-name
                          'attr-index attr-index
                          'attr-subindex nil))
      (abook-contact-display-attribute-type attr-index)
      (insert ":")
      (insert " ")
      ;; Insert attribute value
      (insert (propertize (car attr-value)
                          'face 'abook-attribute-value
                          'attr-index attr-index
                          'attr-subindex nil))
      (insert "\n"))

    (setq attr-region-end (point))
    (put-text-property attr-region-begin attr-region-end
                       'attr-region attr-index)))

(defun abook-contact-display-attribute-type (attr-index)
  (let* ((card (abook-get-card abook-current-card))
         (attr (vcard-get-attribute card attr-index))
         (attr-name (vcard-attr-get-name attr))
         (property (abook-get-property attr-name))
         (type-param (abook-get-prop-parameter property "type"))
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
               (abook-list-to-csv printable-type-list)
               'face 'abook-attribute-type))
      (insert ")"))))

(defun abook-contact-group-hidden-p (group)
  (save-excursion
    (let ((group-exist (abook-contact-goto-group group)))
      (and group-exist
           (get-text-property group-exist 'invisible)))))

(defun abook-contact-hide-show-group (group show-p)
  "Hide GROUP attributes from the screen"
  (save-excursion
    (let ((group-exist (abook-contact-goto-group group))
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

(defun abook-contact-get-current-group ()
  "Return the group affecting current buffer point, or nil"
  (let ((prop-change-pos (previous-single-property-change
                          (point) 'group)))
    (when prop-change-pos
      (save-excursion
        (goto-char (- prop-change-pos 1))
        (get-text-property (point) 'group)))))

(defun abook-contact-get-current-attr-index ()
  "Return the attribute index of the attribute displayed in the current line"
  (abook-get-text-property-line 'attr-index))

(defun abook-contact-get-current-attr-subindex ()
  "Return the attribute subindex of the attribute displayed in the current line"
  (abook-get-text-property-line 'attr-subindex))

(defun abook-contact-get-current-attr-compound-title ()
  (abook-get-text-property-line 'attr-compound-title))

(defun abook-contact-goto-group (group)
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

(defun abook-contact-redisplay-card ()
  "Redisplay current card"
  (erase-buffer)
  (abook-contact-display-card abook-current-card))

(defun abook-contact-redisplay-group (group)
  "Redisplay GROUP in the screen"
  (save-excursion
    (let ((group-exist (abook-contact-goto-group group)))
      (when group-exist
        ;; Remove old group contents
        (abook-contact-erase-group-region)
        ;; Display the group
        (abook-contact-display-group (abook-get-group group))))))

(defun abook-contact-erase-group-region ()
  "Erase the region used by the group in point"
  (abook-erase-tagged-region 'group-region))

(defun abook-contact-erase-attr-region ()
  "Erase the region used by the attribute in point"
  (abook-erase-tagged-region 'attr-region))

(defun abook-contact-redisplay-attr-at-point ()
  "Redisplay the attribute at point"
  (let* ((column-backup (current-column))
         (line-backup (line-number-at-pos (point)))
         (group-symbol (abook-contact-get-current-group))
         (attr-index (abook-contact-get-current-attr-index)))
    (if (and group-symbol attr-index)
      (let* ((card (abook-get-card abook-current-card))
             (group (abook-get-group group-symbol))
             (group-attrs (abook-get-group-props group))
             group-aregion-begin group-region-end)
        (abook-contact-erase-attr-region)
        (setq group-region-begin (point))
        (abook-contact-display-attribute attr-index)
        (setq group-region-end (point))
        (put-text-property group-region-begin
                           group-region-end
                           'group-region group-symbol)
        (goto-line line-backup)
        (goto-char (+ (line-beginning-position) column-backup))))))

(defun abook-contact-in-display-p (group-symbol)
  (abook-contact-goto-group group-symbol))

(defun abook-contact-photo-displayed-p ()
  (next-single-property-change (point-min) 'identification-photo))

;; ** Commands

(defun abook-contact-add-attribute-type ()
  "Add a new type to the attribute under point"
  (interactive)
  (let ((buffer-read-only nil)
        (point-backup (point))
        (group-symbol (abook-contact-get-current-group))
        (attr-index (abook-contact-get-current-attr-index))
        (attr-subindex (abook-contact-get-current-attr-subindex)))
    (if (and attr-index
             (not attr-subindex))
        (let* ((card (abook-get-card abook-current-card))
               (attr (vcard-get-attribute card attr-index))
               (attr-name (vcard-attr-get-name attr))
               (property (abook-get-property attr-name))
               (prop-types (abook-get-prop-parameter property "type")))
          (if prop-types
              (let ((new-type (abook-select-non-existing-type attr))
                    type result)
                (dolist (type prop-types)
                  (if (equal (cadr type)
                             new-type)
                      (setq result (car type))))
                (when result
                  (if (abook-prop-parameter-allow-duplicates property "type")
                      ;; Add the new type
                      (vcard-attr-add-property attr "type" result)
                    ;; Replace current type
                    (vcard-attr-set-property attr "type" result))
                  ;; Redisplay attribute
                  (abook-contact-redisplay-attr-at-point)
                  ;; Addressbook modified
                  (add-to-list 'abook-modified-cards abook-current-card))))))
    (goto-char point-backup)))

(defun abook-contact-remove-attribute-type ()
  "Remove a type from the attribute under point"
  (interactive)
  (let ((buffer-read-only nil)
        (point-backup (point))
        (group-symbol (abook-contact-get-current-group))
        (attr-index (abook-contact-get-current-attr-index))
        (attr-subindex (abook-contact-get-current-attr-subindex)))
    (if (and attr-index
             (not attr-subindex))
        (let* ((card (abook-get-card abook-current-card))
               (attr (vcard-get-attribute card attr-index))
               (attr-name (vcard-attr-get-name attr))
               (property (abook-get-property attr-name))
               (prop-types (abook-get-prop-parameter property "type")))
          (if prop-types
              (if (and (equal (length (vcard-attr-get-parameter attr "type")) 1)
                       (abook-prop-parameter-is-mandatory property "type"))
                  (message "This attribute should have a type")
                (let ((new-type (abook-select-existing-type attr))
                      type result)
                  (dolist (type prop-types)
                    (if (equal (cadr type)
                               new-type)
                        (setq result (car type))))
                  (when result
                    ;; Add the new type
                    (vcard-attr-remove-property attr "type" result)
                    ;; Redisplay attribute
                    (abook-contact-redisplay-attr-at-point)
                    ;; Addressbook modified
                    (add-to-list 'abook-modified-cards abook-current-card)))))))
    (goto-char point-backup)))

(defun abook-contact-delete-attribute ()
  "Delete the attribute under point"
  (interactive)
  (let ((buffer-read-only nil)
        (point-backup (point))
        (group-symbol (abook-contact-get-current-group))
        (attr-index (abook-contact-get-current-attr-index))
        (attr-subindex (abook-contact-get-current-attr-subindex)))
  (if (and group-symbol attr-index)
      (let* ((group (abook-get-group group-symbol))
             (group-attrs (abook-get-group-props group))
             (card (abook-get-card abook-current-card))
             (attr (vcard-get-attribute card attr-index))
             (attr-name (vcard-attr-get-name attr))
             (attr-value (vcard-attr-get-values attr))
             (group-attr (abook-get-group-prop group-attrs attr-name))
             (attr-title (abook-get-prop-title group-attr))
             (attr-fields (abook-get-prop-fields-list group-attr))
             (attr-field (when attr-subindex (nth attr-subindex attr-fields)))
             (attr-field-name (when attr-field (abook-get-prop-field-name attr-field)))
             (prompt (concat "Are you sure you want to delete "
                             (if attr-subindex
                                 (concat "field " attr-field-name)
                               (concat "attribute " attr-title))
                             "? "))
             elt)
        (if (yes-or-no-p prompt)
            (if (and (member attr-name abook-required-attrs)
                     (or (abook-contact-get-current-attr-compound-title)
                         (equal (abook-number-of-values attr-value) 1)))
                (error "Trying to delete a required attribute")
              (abook-delete-attr attr-index attr-subindex)
              (if (not (equal attr-name "photo"))
                  (abook-contact-redisplay-group group-symbol)
                (abook-contact-redisplay-card))))))
  (goto-char point-backup)
  (add-to-list 'abook-modified-cards abook-current-card)))

(defun abook-contact-add-attribute ()
  "Add a new attribute to the current card"
  (interactive)
  (let* (buffer-read-only
         (backup-point (point))
         group-symbol
         group group-attrs
         (i 0)
         (current-card (abook-get-card abook-current-card)))
    ;; Get group
    (setq group-symbol (or (abook-contact-get-current-group)
                           (abook-select-group)))
    (setq group (abook-get-group group-symbol))
    (setq group-attrs (abook-get-group-props group))
    (if group-symbol
        (let (attr-index attr-subindex property-index)
          ;; Get property
          (setq attr-index (abook-contact-get-current-attr-index))
          (setq attr-subindex (abook-contact-get-current-attr-subindex))
          (if (and attr-index attr-subindex)
              (let ((attr (vcard-get-attribute current-card attr-index)))
                (setq property-name (vcard-attr-get-name attr)))
            (setq property-name (abook-select-property group-symbol)))
          (if property-name
              (let* ((property (abook-get-group-prop group-attrs property-name))
                     (property-title (abook-get-prop-title property))
                     (property-fields (abook-get-prop-fields property))
                     field-index (property-value "") prompt
                     (continue t))
                ;; Get field
                (when property-fields
                  (setq field-index (abook-select-field group-symbol property-name))
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
                    (let* ((new-attr-type (abook-get-prop-default-type property-name))
                           (new-attr-name property-name)
                           (new-attr-values property-value)
                           new-attr)
                      (setq new-attr (list (list new-attr-name)
                                           new-attr-values))
                      (vcard-attr-set-property new-attr "type" new-attr-type)
                      (if (equal new-attr-name "photo")
                          (vcard-attr-set-property new-attr "value" "url"))
                      (setq current-card (vcard-add-attribute current-card new-attr))))
                  (abook-set-card abook-current-card current-card)
                  (if (abook-contact-in-display-p group-symbol)
                      (progn
                        ;; Redisplay the group with new contents
                        (abook-contact-redisplay-group group-symbol)
                        ;; Hide the group if it was hidden
                        (if (abook-contact-group-hidden-p group)
                            (abook-contact-hide-show-group group nil)))
                    ;; Redisplay the entire card
                    (abook-contact-redisplay-card))
                  ;; This card has been modified
                  (add-to-list 'abook-modified-cards abook-current-card))))))
    (goto-char backup-point)))

(defun abook-contact-edit-attribute ()
  (interactive)
  "Edit the value of the attribute located in the current line"
  (let ((buffer-read-only nil)
        (group-symbol (abook-contact-get-current-group))
        (attr-index (abook-contact-get-current-attr-index))
        (attr-subindex (abook-contact-get-current-attr-subindex))
        (attr-compound-title-p (abook-contact-get-current-attr-compound-title)))
    (if (and group-symbol attr-index (not attr-compound-title-p))
        (let* ((group (abook-get-group group-symbol))
               (group-attrs (abook-get-group-props group))
               (card (abook-get-card abook-current-card))
               (attr (vcard-get-attribute card attr-index))
               (attr-name (vcard-attr-get-name attr))
               (attr-value (vcard-attr-get-values attr))
               (group-attr (abook-get-group-prop group-attrs attr-name))
               (attr-fields (abook-get-prop-fields group-attr))
               (attr-real-value (if attr-subindex
                                    (nth attr-subindex attr-value)
                                  (car attr-value)))
               (attr-real-name (if attr-subindex
                                   (nth attr-subindex attr-fields)
                                 (abook-get-prop-title group-attr)))
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
          (add-to-list 'abook-modified-cards abook-current-card)
          ;; Redisplay attribute
          ;; FIXME: use abook-contact-redisplay-attr-at-point
          (let ((column-backup (current-column))
                (line-backup (line-number-at-pos (point))))
            (abook-contact-erase-attr-region)
            (abook-contact-display-attribute attr-index)
            (goto-line line-backup)
            (goto-char (+ (line-beginning-position) column-backup)))))))

(defun abook-contact-goto-next-group ()
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

(defun abook-contact-toggle-hide-show-group ()
  "When staying on a parameters group title, toggle visibility of the group"
  (interactive)
  (let ((buffer-read-only nil)
        (group (get-text-property (point) 'group))
        (group-content-pos nil))
    (when group
      ;; Search for visibility properties in group contents
      (setq group-content-pos (next-single-property-change (point) 'group))
      (if (get-text-property group-content-pos 'invisible)
          (abook-contact-hide-show-group group t)
        (abook-contact-hide-show-group group nil)))))

(defun abook-contact-hide-all-groups ()
  "Hide all displayed groups"
  (interactive)
  (let (buffer-read-only)
    (dolist (group abook-contact-displayed-groups nil)
      (abook-contact-hide-show-group group nil))))

(defun abook-contact-show-all-groups ()
  "Show all displayed groups"
  (interactive)
  (let (buffer-read-only)
    (dolist (group abook-contact-displayed-groups nil)
      (abook-contact-hide-show-group group t))))

(defun abook-contact-cycle-groups ()
  "Cycle to next group"
  (interactive)
  (let ((next-group-pos (abook-contact-goto-next-group)))
    (when (not next-group-pos)
      (goto-char (point-min))
      (abook-contact-goto-next-group))))

;; ** Modeline management

(defun abook-contact-set-mode-line (card-number total-cards)
  "Update the modeline of the current buffer"
  ;; FIXME: this is ugly
  (when abook-contact-mode-line-string
    (setq mode-line-buffer-identification
	  (list 24
              abook-contact-mode-line-string
              ": "
              (list 10
                    (format "%d/%d" card-number total-cards))))))

;; ** Major mode
(defun abook-contact-mode ()
      "A major mode for contact editing

Commands:
\\{abook-contact-mode-map}"
      (interactive)
      (kill-all-local-variables)
      (setq abook-contact-mode-map (make-keymap))
      (define-key abook-contact-mode-map "c" 'abook-create-contact)
      (define-key abook-contact-mode-map "D" 'abook-delete-card)
      (define-key abook-contact-mode-map "n" 'abook-next-contact)
      (define-key abook-contact-mode-map "p" 'abook-previous-contact)
      (define-key abook-contact-mode-map "s" 'abook-save-cards)
      (define-key abook-contact-mode-map "x" 'abook-export-card)
      (define-key abook-contact-mode-map "b" 'abook-bury)
      (define-key abook-contact-mode-map "q" 'abook-quit)
      (define-key abook-contact-mode-map "e" 'abook-contact-edit-attribute)
      (define-key abook-contact-mode-map (kbd "SPC") 'abook-contact-toggle-hide-show-group)
      (define-key abook-contact-mode-map (kbd "TAB") 'abook-contact-cycle-groups)
      (define-key abook-contact-mode-map "d" 'abook-contact-delete-attribute)
      (define-key abook-contact-mode-map "a" 'abook-contact-add-attribute)
      (define-key abook-contact-mode-map "t" 'abook-contact-add-attribute-type)
      (define-key abook-contact-mode-map "r" 'abook-contact-remove-attribute-type)
      (define-key abook-contact-mode-map "m" 'abook-send-email)
      (define-key abook-contact-mode-map "H" 'abook-contact-hide-all-groups)
      (define-key abook-contact-mode-map "S" 'abook-contact-show-all-groups)
      (define-key abook-contact-mode-map "h" 'abook-summarize)
      (use-local-map abook-contact-mode-map)
      (setq mode-name "ABook Contact")
      (setq major-mode 'abook-contact-mode))

;; * Addressbook Summary

;; ** Constants

(defconst abook-summary-buffer-name "*ABook Summary*"
  "Name of the buffer for the addressbook summary")

;; ** Variables

(defvar abook-summary-mode-map nil
  "Keymap for abook-summary-mode")

(defvar abook-summary-mode-line-string " ABook Summary"
  "String to display on the mode line when in the addressbook summary mode.
If `nil', do not show anything.")

;; ** Summary buffer management

(defun abook-make-summary-buffer ()
  (save-excursion
    (let ((buffer (get-buffer-create abook-summary-buffer-name)))
      (set-buffer buffer)
      (abook-summary-mode)
      (abook-summary-display)
      (setq buffer-read-only t)
      (setq abook-summary-buffer buffer)
      buffer)))

(defun abook-summary ()
  "Open the addressbook and show the summary window"
  (let ((buffer (get-buffer abook-summary-buffer-name)))
    (when (not buffer)
      (setq buffer (abook-make-summary-buffer)))
    (switch-to-buffer-other-window abook-summary-buffer)
    ;; The overlay set by the following function is not working when
    ;; installed here... wtf???
    (abook-summary-goto-contact 0 t)))


(defun abook-summarize ()
  "Summarize the contents of the addressbook in a summary buffer.

The format is as described in the variable `abook-summary-format'"
  (interactive)
  (if (not (get-buffer abook-summary-buffer-name))
      (save-excursion
        (abook-get-create-summary-buffer)
        (set-buffer (get-buffer abook-summary-buffer-name))
        (abook-summary-goto-contact abook-current-card nil)))
  (abook-show-summary))

(defun abook-show-summary ()
  (let ((buffer (get-buffer abook-summary-buffer-name)))
    (if abook-use-multiple-frames
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))
    (abook-summary-refresh)
    (abook-summary-goto-contact abook-current-card nil)))

(defun abook-get-create-summary-buffer ()
  (if (not abook-summary-buffer)
      (save-excursion
        (setq abook-summary-buffer (get-buffer-create abook-summary-buffer-name))
        (set-buffer abook-summary-buffer)
        (abook-summary-mode)
        (abook-summary-display)))
  abook-summary-buffer)

;; ** Display functions

(defun abook-summary-display ()
  (erase-buffer)
  (let (card-index card name)
    (dotimes (card-index (length abook-cards))
      (insert "  ")
      (insert " ")
      (insert (make-string (- 4 (length (number-to-string (+ card-index 1)))) ?\ ))
      (insert (propertize (number-to-string (+ card-index 1))
                          'face 'abook-summary-card-number)
              " ")
      (insert " ")
      (setq card (abook-get-card card-index))
      (setq name (vcard-get-named-attribute card "n"))
      (insert (propertize (abook-get-card-fn t card-index)
                          'face 'abook-attribute-value))
      (add-text-properties (line-beginning-position)
                           (line-end-position)
                           (list 'card-index card-index))
      (insert (propertize "\n"
                          'card-index card-index)))))

(defun abook-summary-goto-contact (numcard update-contact-buffer)
  (let (new-pos temp-new-pos found)
    (remove-overlays (point-min) (point-max))
    (if (equal (get-text-property (point-min) 'card-index) numcard)
        (setq new-pos (point-min))
      (setq temp-new-pos (point-min))
      (while (and (not found)
                  (setq temp-new-pos (next-single-property-change temp-new-pos 'card-index)))
        (when (equal (get-text-property temp-new-pos 'card-index) numcard)
          (setq new-pos temp-new-pos)
          (setq found t))))
    (when new-pos
      (goto-char new-pos)
      (beginning-of-line)
      (let ((highlight-overlay (make-overlay (line-beginning-position)
                                             (line-beginning-position 2))))
        (overlay-put highlight-overlay 'face 'abook-summary-selected-card))
      (abook-summary-set-mode-line (+ numcard 1) (length abook-cards))
      (when (and update-contact-buffer
                 (get-buffer abook-contact-buffer-name))
        (save-excursion
          (set-buffer (get-buffer abook-contact-buffer-name))
          (let (buffer-read-only)
            (abook-contact-display-card numcard)))))))

(defun abook-summary-get-current-card ()
  (get-text-property (point) 'card-index))

(defun abook-summary-refresh ()
  "Refresh the summary screen"
  (let ((card-index (abook-summary-get-current-card))
        (column-backup (current-column))
        (buffer-read-only nil))
    (abook-summary-display)
    (abook-summary-goto-contact card-index t)
    (goto-char (+ (line-beginning-position) column-backup))))

;; ** Commands

(defun abook-summary-next-contact ()
  "Select the next card in the summary buffer"
  (interactive)
  (let ((card-index (abook-summary-get-current-card)))
    (cond
     ((equal card-index (- (length abook-cards) 1))
      (abook-summary-goto-contact 0 t))
     (t
      (abook-summary-goto-contact (+ card-index 1) t)))))

(defun abook-summary-previous-contact ()
  "Select the previous card in the summary buffer"
  (interactive)
  (let ((card-index (abook-summary-get-current-card)))
    (cond
     ((equal card-index 0)
      (abook-summary-goto-contact (- (length abook-cards) 1) t))
     (t
      (abook-summary-goto-contact (- card-index 1) t)))))

(defun abook-summary-create-contact ()
  ""
  (interactive)
  (save-excursion
    (set-buffer (get-buffer-create abook-contact-buffer-name))
    (abook-create-contact))
  (abook-summary-refresh))

(defun abook-summary-import-vcard (filename)
  "Import vCard from FILENAME and add it into our contact database and return the
index of the last imported card from the file."
  (interactive
   (list
    (expand-file-name
     (read-file-name "vCard file to import: "))))

  (abook-import-vcard filename)
  (abook-summary-refresh))

(defun abook-summary-show-contact ()
  "Open an addressbook buffer to show the current selected card"
  (interactive)
  (let ((card-index (abook-summary-get-current-card)))
    (when (not (get-buffer abook-contact-buffer-name))
      (save-excursion
        (abook-create-contact-buffer)
        (abook-contact-display-card card-index)
        (setq buffer-read-only t)))
    (abook-show-contact)))

;; Modeline management

(defun abook-summary-set-mode-line (card-number total-cards)
  "Update the mdoeline of the current summary buffer"
  ;; FIXME: this is ugly
  (when abook-summary-mode-line-string
    (setq mode-line-buffer-identification
	  (list 24
		abook-summary-mode-line-string
		": "
		(list 10
		      (format "%d/%d" card-number total-cards))))))

;; ** Major mode

(defun abook-summary-mode ()
  "A major mode for the addressbook summary window

Commands:
\\{abook-summary-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq abook-summary-mode-map (make-keymap))
  (define-key abook-summary-mode-map "n" 'abook-summary-next-contact)
  (define-key abook-summary-mode-map "p" 'abook-summary-previous-contact)
  (define-key abook-summary-mode-map (kbd "<down>") 'abook-summary-next-contact)
  (define-key abook-summary-mode-map (kbd "<up>") 'abook-summary-previous-contact)
  (define-key abook-summary-mode-map (kbd "RET") 'abook-summary-show-contact)
  (define-key abook-summary-mode-map "b" 'abook-bury)
  (define-key abook-summary-mode-map "q" 'abook-quit)
  (define-key abook-summary-mode-map "c" 'abook-summary-create-contact)
  (define-key abook-summary-mode-map "i" 'abook-summary-import-vcard)
  (define-key abook-summary-mode-map "x" 'abook-export-vcard)
  (define-key abook-summary-mode-map "m" 'abook-send-email)
  (define-key abook-summary-mode-map "d" 'abook-summary-delete-contact)
  (use-local-map abook-summary-mode-map)
  (setq mode-name "ABook Summary")
  (setq major-mode 'abook-summary-mode))

;; * General commands (usable from all addressbook modes)
1
(defun abook-send-email ()
  "Send an email to current contact"
  (interactive)
  (let* ((card (abook-get-card abook-current-card))
         (mail-addresses (vcard-ref card (list "email")))
         mail-names name i attr sendto-address letter)
    (dotimes (i (length mail-addresses))
      (let* ((attr (nth i mail-addresses))
             (attr-type (car (vcard-attr-get-parameter attr "type"))))
        (if (equal attr-type "internet")
            (setq mail-names (cons (list (car (vcard-attr-get-values attr))
                                         (+ ?a i))
                                   mail-names)))))
    (setq mail-names (reverse mail-names))
    (if (not mail-names)
        (message "Contact doesnt have a suitable smtp address")
      (if (equal (length mail-names) 1)
          (setq sendto-address (car (car mail-names)))
        (setq letter (abook-fast-selection mail-names "Select email address to send mail to"))
        (dolist (name mail-names)
          (if (equal letter
                     (cadr name))
              (setq sendto-address (car name)))))
      ;; Send the email
      (if sendto-address
          (compose-mail-other-frame (concat
                                     "\"" (abook-get-card-fn) "\""
                                     " <" sendto-address ">"))))))

(defun abook-delete-card ()
  "Delete the current card"
  (interactive)
  (let ((buffer-read-only nil)
        (current-card abook-current-card)
        (prompt "Are you sure you want to delete current contact? "))
    (when (yes-or-no-p prompt)
      (if (equal current-card (- (length abook-cards) 1))
          (setq current-card (- (length abook-cards) 2)))
      (abook-remove-card abook-current-card)
      (add-to-list 'abook-modified-cards current-card)
      (if (equal (length abook-cards) 0)
          (abook-quit)
        (abook-contact-display-card current-card)))))

(defun abook-create-contact ()
  "Create a new card"
  (interactive)
  (let ((buffer-read-only nil)
        (new-card-index (abook-create-contact-2)))
    (if new-card-index
        (abook-contact-display-card new-card-index))))

(defun abook-create-contact-2 ()
  "Create a new card with minimum identification properties and insert it
into `abook-cards'.

Return the index position of the new card"
  (let* (new-card
         (n-surname (read-from-minibuffer "Surname: "))
         (n-first-name (read-from-minibuffer "First name: "))
         (n-aka (read-from-minibuffer "AKA: "))
         (n-name-prefix (read-from-minibuffer "Name prefix: "))
         (n-name-suffix (read-from-minibuffer "Name suffix: "))
         (no-values (and (equal n-surname "")
                         (equal n-first-name "")
                         (equal n-aka "")
                         (equal n-name-prefix "")
                         (equal n-name-suffix "")))
         (new-card-index (length abook-cards)))
    (if no-values
        (progn
          (message "Contact not created")
          nil)
      ;; Create a new card
      (setq new-card (vcard-add-attribute new-card
                                          (cons (list "n")
                                                (list n-surname
                                                      n-first-name
                                                      n-aka
                                                      n-name-prefix
                                                      n-name-suffix))))
      (abook-set-card new-card-index new-card)
      (add-to-list 'abook-modified-cards new-card-index)
      new-card-index)))

(defun abook-import-vcard (filename)
  "Import vCard from FILENAME and add it into our contact database and return the
index of the last imported card from the file."
  (let ((index nil)
	vcard)
    (abook-be-read-cards)
    (save-excursion
      (unwind-protect
	  (if (and (setq index (length abook-cards))
		   (setq vcard (vcard-parse-file filename)))
              (dolist (new-card vcard)
                (abook-set-card index new-card)
                (add-to-list 'abook-modified-cards index)
                ;; FIXME: update the summary buffer, if it exists
                (setq index (1+ index)))
	    (error "Vcard import failed!"))
	;; Just to be sure, call save-cards
	(abook-save-cards nil)))
    index))

(defun abook-summary-delete-contact ()
  "Delete the contact under point in the summary buffer."
  (interactive)
  (let ((abook-current-card (abook-summary-get-current-card)))
    (abook-delete-card)))

;; FIXME: does not work in contact mode
(defun abook-export-vcard ()
  "Export current card data to a file."
  (interactive)
  (let* ((index (abook-summary-get-current-card))
	 (fullname (abook-get-card-fn nil index ))
	 (filename (read-file-name "Export vCard to file: " nil nil
				   nil (concat fullname ".vcf"))))
    (abook-write-data-1 filename (abook-get-card index))
    (message "vCard exported")))

(defun abook-write-data-1 (filename &optional vcard)
  "Save raw vCard formatted data into FILENAME.
If optional VCARD parameter is not set, use `abook-current-card'."
  (let ((vcard (or vcard (abook-get-card abook-current-card))))
    (with-temp-file filename
      (vcard-insert vcard))))

(defun abook-send-email ()
  "Send an email to current contact"
  (interactive)
  (let* ((card (abook-get-card abook-current-card))
         (mail-addresses (vcard-ref card (list "email")))
         mail-names name i attr sendto-address letter)
    (dotimes (i (length mail-addresses))
      (let* ((attr (nth i mail-addresses))
             (attr-type (car (vcard-attr-get-parameter attr "type"))))
        (if (equal attr-type "internet")
            (setq mail-names (cons (list (car (vcard-attr-get-values attr))
                                         (+ ?a i))
                                   mail-names)))))
    (setq mail-names (reverse mail-names))
    (if (not mail-names)
        (message "Contact doesnt have a suitable smtp address")
      (if (equal (length mail-names) 1)
          (setq sendto-address (car (car mail-names)))
        (setq letter (abook-fast-selection mail-names "Select email address to send mail to"))
        (dolist (name mail-names)
          (if (equal letter
                     (cadr name))
              (setq sendto-address (car name)))))
      ;; Send the email
      (if sendto-address
          (compose-mail-other-frame (concat
                                     "\"" (abook-get-card-fn) "\""
                                     " <" sendto-address ">"))))))

(defun abook-save-cards (prefix)
  "Save cards into abook-file"
  (interactive "P")
  (if prefix
      (abook-export-card)
    ;; Save modified cards into addressbook-file
    (if (equal (length abook-modified-cards) 0)
        (message "addressbook not saved")
      (let ((i 0))
        (dotimes (i (length abook-cards))
          (when (member i abook-modified-cards)
            (abook-be-write-card i))))
      (setq abook-modified-cards nil)
      (set-buffer-modified-p nil)
      (message "addressbook saved"))))

(defun abook-next-contact ()
  "Display the next card"
  (interactive)
  (let (buffer-read-only window-list win)
    (if (equal abook-current-card (- (length abook-cards) 1))
        (message "No more cards")
      (abook-contact-display-card (+ abook-current-card 1))
      (let ((summary-buffer (get-buffer abook-summary-buffer)))
        (when summary-buffer
          (setq window-list (get-buffer-window-list summary-buffer nil t))
          (dolist (win window-list)
            (with-selected-window (get-buffer-window summary-buffer t)
              (abook-summary-goto-contact abook-current-card nil))))))))

(defun abook-previous-contact ()
  "Display the previous card"
  (interactive)
  (let (buffer-read-only)
    (if (equal abook-current-card 0)
        (message "First card")
      (abook-contact-display-card (- abook-current-card 1))
      (let ((summary-buffer (get-buffer abook-summary-buffer)))
        (when summary-buffer
          (setq window-list (get-buffer-window-list summary-buffer nil t))
          (dolist (win window-list)
            (with-selected-window (get-buffer-window summary-buffer t)
              (abook-summary-goto-contact abook-current-card nil))))))))

(defun abook-quit ()
  "Exit the addressbook."
  (interactive)
  (if (and (not (equal (length abook-modified-cards) 0))
           (yes-or-no-p "Save addressbook? "))
      (abook-save-cards nil))
  (let ((contact-buffer (get-buffer abook-contact-buffer-name))
        (summary-buffer (get-buffer abook-summary-buffer-name))
        win window-list)
    (when summary-buffer
      ;; Delete windows (and possibly frames)
      (delete-windows-on summary-buffer)
      (kill-buffer summary-buffer))
    (when contact-buffer
      ;; Delete windows (and possibly frames)
      (delete-windows-on contact-buffer)
      (kill-buffer contact-buffer))))

(defun abook-bury ()
  "Bury the addressbook buffer(s)."
  (interactive)
  (when (or (eq major-mode 'abook-summary-mode)
	    (eq major-mode 'abook-contact-mode ))
    (bury-buffer)))

(defun abook-export-card ()
  "Export current card data to a file"
  (interactive)
  (let ((filename (read-file-name "Export vCard to file: "))
        (card (abook-get-card abook-current-card)))
    (with-temp-file filename
      (vcard-insert card))
    (message "vCard exported")))

;; * Backend management

;; ** Customization and Variables

(defcustom abook-backend
  'abook-backend-simple
  "Backend to use for the addressbook. 

Currently there are two backends available: `abook-backend-simple' (simple backend
to store all contacts in one file) and `abook-backend-multiple' (that stores one contact per file in
a given directory"
  :type 'symbol)

;; ** Utility functions

(defun abook-make-params-explicit ()
  "Make unambiguous anonymous params explicit.

It uses `abook-general-params' and the type parameter for each property
defined in `abook-properties'"
  (let ((i 0))
    (dolist (card abook-cards)
      (dotimes (i (vcard-get-num-attributes card))
        (let* ((attr (vcard-get-attribute card i))
               (attr-name (vcard-attr-get-name attr))
               (attr-props (cdr (vcard-attr-get-proplist attr)))
               (property (abook-get-property attr-name))
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
                  (let* ((general-param (assoc param-value abook-general-params))
                         (general-param-name (if general-param (cadr general-param)))
                         (prop-types (abook-get-prop-parameter property "type")))
                    (if general-param-name
                        (setq param-name general-param-name)
                      (if (and prop-types
                               (assoc param-value prop-types))
                          (setq param-name "type")))
                    (if param-name
                        (setcar (nthcdr j attr-props) (cons param-name param-value))))))))))))

;; ** API

(defun abook-be-read-cards ()
  "Read cards from an addressbook backend.

This function stores the retrieved vCard information in 
`abook-cards'."
  (cond
   ((equal abook-backend 'abook-backend-simple)
    (abook-be-simple-read-cards))
   ((equal abook-backend 'abook-backend-multiple)
    (abook-be-multiple-read-cards))
   (t
    (error "No valid addressbook backend selected.")))
  (when abook-cards
    (abook-make-params-explicit)
    t))

(defun abook-be-write-card (card-id)
  "Write the CARD-ID card to the appropiate backend."
  (cond
   ((equal abook-backend 'abook-backend-simple)
    (abook-be-simple-write-card card-id))
   ((equal abook-backend 'abook-backend-multiple)
    (abook-be-multiple-write-card card-id))
   (t
    (error "No valid addressbook backend selected."))))

(defun abook-be-delete-card (card-id)
  "Delete the CARD-ID card from the appropiate backend."
  (cond
   ((equal abook-backend 'abook-backend-simple)
    (abook-be-simple-delete-card card-id))
   ((equal abook-backend 'abook-backend-multiple)
    (abook-be-multiple-delete-card card-id))
   (t
    (error "No valid addressbook backend selected."))))
  
;; ** Simple backend

(defcustom abook-file "~/.abook"
  "File with stored addresses"
  :type 'file
  :group 'abook)

(defun abook-be-simple-read-cards ()
  "Read cards from addressbook file"
  (with-temp-buffer
    (insert-file-contents abook-file)
    (setq abook-cards (vcard-parse-region (point-min)
                                             (point-max)))))

(defun abook-be-simple-write-card (card-id)
  "Write cards information to `abook-file', discarding any
previous content."
  (with-temp-file abook-file
    (dotimes (i (length abook-cards))
      (let ((card (abook-get-card i)))
        (vcard-insert card)
        (if (not (equal i (- (length abook-cards) 1)))
            (insert "\n\n"))))))

;; * Utility functions

(defun abook-list-to-csv (list)
  (let ((result "")
        i)
    (dotimes (i (length list))
      (setq result (concat result (nth i list)))
      (if (not (equal i (- (length list) 1)))
          (setq result (concat result ","))))
    result))

(defun abook-open ()
  "Open the addressbook"
  (or (abook-be-read-cards)
      (abook-create-contact-2)))

(defun abook-get-text-property-line (prop)
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

(defun abook-erase-tagged-region (tag)
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

(defun abook-sort-cards ()
  "Sort `abook-cards' using the `abook-field-for-sort' field"
  (setq abook-cards
        (sort abook-cards
              (lambda (card1 card2)
                (let* ((card1-n (vcard-get-named-attribute card1 "n"))
                       (card2-n (vcard-get-named-attribute card2 "n"))
                       (n-prop (abook-get-property "n"))
                       (n-fields (abook-get-prop-fields n-prop))
                       (field-index (abook-get-prop-index n-fields abook-field-for-sort))
                       (card1-n-field (nth field-index (vcard-attr-get-values card1-n)))
                       (card2-n-field (nth field-index (vcard-attr-get-values card2-n))))
                  (cond
                   ((and (null card1-n-field) (not (null card2-n-field)))
                    t)
                   ((and (not (null card1-n-field)) (null card2-n-field))
                    nil)
                   ((and (null card1-n-field) (null card2-n-field))
                    t)
                   (t
                    (string-lessp card1-n-field card2-n-field))))))))

;; * Fast selection

(defun abook-fast-selection (names prompt)
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
      (set-buffer (get-buffer-create " *Abook Groups*"))
      (delete-other-windows)
      (split-window-vertically)
      (switch-to-buffer-other-window (get-buffer-create " *Abook Groups*"))
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

(defun abook-select-type (attr-name)
  (let* ((property (abook-get-property attr-name))
         (prop-types (abook-get-prop-parameter property "type")))
    (let (type-names type letter result)
      (dolist (type prop-types)
        (setq type-names
              (cons (cdr type) type-names)))
      (setq type-names (reverse type-names))
      (setq letter (abook-fast-selection type-names "Select attribute type"))
      (if letter
          (dolist (type type-names)
            (if (equal letter
                       (cadr type))
                (setq result (car type)))))
      result)))

(defun abook-select-non-existing-type (attr)
  (let* ((attr-name (vcard-attr-get-name attr))
         (property (abook-get-property attr-name))
         (prop-types (abook-get-prop-parameter property "type"))
         (attr-types (vcard-attr-get-parameter attr "type")))
    (let (type-names type letter result)
      (dolist (type prop-types)
        (if (not (member (car type) attr-types))
            (setq type-names
                  (cons (cdr type) type-names))))
      (setq type-names (reverse type-names))
      (setq letter (abook-fast-selection type-names "Select attribute type"))
      (if letter
          (dolist (type type-names)
            (if (equal letter
                       (cadr type))
                (setq result (car type)))))
      result)))

(defun abook-select-existing-type (attr)
  (let* ((attr-name (vcard-attr-get-name attr))
         (property (abook-get-property attr-name))
         (prop-types (abook-get-prop-parameter property "type"))
         (attr-types (vcard-attr-get-parameter attr "type")))
    (let (type-names type letter result)
      (dolist (type prop-types)
        (if (member (car type) attr-types)
            (setq type-names
                  (cons (cdr type) type-names))))
      (setq type-names (reverse type-names))
      (setq letter (abook-fast-selection type-names "Select attribute type"))
      (if letter
          (dolist (type type-names)
            (if (equal letter
                       (cadr type))
                (setq result (car type)))))
      result)))

(defun abook-select-group ()
  "Select a group interactively and return its symbol"
  (let (names group group-elt letter result)
    ;; Build the names list
    (dolist (group-elt abook-properties)
      (setq names
            (cons (list (abook-get-group-name group-elt)
                        (abook-get-group-letter group-elt))
                  names)))
    (setq names (reverse names))
    ;; Call the fast menu function to get the desired group
    (setq letter (abook-fast-selection names "Select group"))
    (dolist (group-elt abook-properties)
      (if (and (abook-get-group-letter group-elt)
               (equal letter (abook-get-group-letter group-elt)))
          (setq result (abook-get-group-symbol group-elt))))
    result))

(defun abook-select-property (group-symbol)
  "Select a property interactively from GROUP and return its name"
  (let* ((group (abook-get-group group-symbol))
         (group-props (abook-get-group-props group))
         names attr attr-elt letter result)
    ;; Build the names list
    (dolist (prop group-props)
      (if (and (not (member (abook-get-prop-name prop) abook-required-attrs))
               (abook-get-prop-letter prop))
          (setq names
                (cons (list (abook-get-prop-title prop)
                            (abook-get-prop-letter prop))
                      names))))
    (setq names (reverse names))
    ;; Call the fast menu function to get the desired group
    (setq letter (abook-fast-selection names "Select property"))
    (dolist (prop group-props)
      (if (and (abook-get-prop-letter prop)
               (equal letter (abook-get-prop-letter prop)))
          (setq result (abook-get-prop-name prop))))
    result))

(defun abook-select-field (group-symbol prop-name)
  "Select a field interactively from PROP-NAME"
  (let* ((group (abook-get-group group-symbol))
         (group-props (abook-get-group-props group))
         (property (assoc prop-name group-props))
         (prop-fields (abook-get-prop-fields-list property))
         letter field result i)
    (setq letter (abook-fast-selection prop-fields "Select property field"))
    (dotimes (i (length prop-fields))
      (setq field (nth i prop-fields))
      (if (equal letter (abook-get-prop-field-letter field))
          (setq result i)))
    result))

;; ** Search functions

(defun abook-attr-matches-p (attr regexp)
  (let (result value
        (attr-values (vcard-attr-get-values attr)))
    (if (listp attr-values)
        (dolist (value attr-values)
          (if (string-match regexp value)
              (setq result t)))
      (setq result (string-match regexp attr-values)))
    result))

(defun abook-search-cards (regexp &optional properties)
  "Search for REGEXP in card data and return a list with the indexes
of matching cards.

PROPERTIES is a list of property names.
If PROPERTIES is specified and non-nil, the search is performed only in those
attributes."
  (let (card prop attr card-index attr-index result)
    (dotimes (card-index (length abook-cards))
      (setq card (abook-get-card card-index))
      (dotimes (attr-index (vcard-get-num-attributes card))
        (setq attr (vcard-get-attribute card attr-index))
        (if (and (or (not properties)
                     (member (vcard-attr-get-name attr) properties))
                 (abook-attr-matches-p attr regexp))
            (add-to-list 'result card-index))))
    (reverse result)))

;; * Entry points to the addressbook

;;;###autoload
(defun abook ()
  "Open the addressbook"
  (interactive)
  (if (and abook-force-addressbook-creation
           (not (file-exists-p abook-file)))
      (with-temp-file abook-file))
  (catch 'exit
    (let ((buffer (get-buffer abook-contact-buffer-name)))
      (if (not buffer)
        (let ((show-card-index 0)
              (user-input (if abook-ask-for-search
                              (read-from-minibuffer "Search for contact [RET goes to the summary]: ")
                            "")))
          (unless (abook-open)
            (throw 'exit t))
          (abook-sort-cards)
          (if (not (equal user-input ""))
              (let ((found-cards (abook-search-cards user-input)))
                (if found-cards
                    (setq show-card-index (car found-cards))
                  (message "No contacts found")
                  (throw 'exit t))
                ;; Goto the first card with matched data
                (abook-create-contact-buffer)
                (abook-contact-display-card show-card-index)
                (setq abook-modified-cards nil)
                (switch-to-buffer-other-window (get-buffer abook-contact-buffer-name))
                (setq buffer-read-only t)
                (setq abook-buffer buffer))
            ;; Goto the summary
            (abook-summary))))
      (abook-show-summary))))


;;;###autoload
(defun abook-create ()
  "Create a new contact into the addressbook and save it"
  (interactive)
  (abook-be-read-cards)
  (let ((new-card-index (abook-create-contact-2)))
    (if new-card-index
        (abook-save-cards nil))))


(provide 'abook)

;; Local variables:
;; outline-regexp: ";; \\*"
;; End:

;;; abook.el ends here
