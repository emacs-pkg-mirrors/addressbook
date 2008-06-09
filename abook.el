;;;; addressbook.el --- A simple addressbook using vcard-mode

;; Copyright (C) 2007, 2008 Jose E. Marchesi

;; Maintainer: Jose E. Marchesi <jemarch@gnu.org>
;; Keywords: contacts, applications

;; $Id: abook.el,v 1.2 2008/06/09 20:38:17 jemarch Exp $

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;;; Commentary:

;; A simple vCard based addressbook for Emacs
;;
;; File Contents
;; =============
;;
;; * Constants
;; * Customization
;; * Variables
;;
;; * Properties management functions
;;
;; ** Groups
;; ** Properties
;; ** Cards
;; ** Attributes
;;
;; * Addressbook contact editor
;;
;; ** Constants
;; ** Variables
;; ** Contact buffer management
;; ** Display functions
;; ** Modeline management
;; ** Commands
;; ** Major mode
;;
;; * Addressbook summary
;;
;; ** Constants
;; ** Variables
;; ** Summary buffer management
;; ** Display functions
;; ** Commands
;; ** Modeline management
;; ** Major mode
;;
;; * General commands (usable from all addressbook modes)
;; * Backend management
;; 
;; ** Customization and Variables
;; ** Utility functions
;; ** API
;; ** Simple backend
;; ** Multiple backend
;;
;; * Utility functions
;;
;; ** Fast selection
;; ** Search functions
;;
;; * Entry points to the addressbook

;;; Code:

(eval-when-compile (require 'cl))
(require 'vcard)
(require 'mm-decode)

;;;; * Constants

(defconst addrbook-version "0.1"
  "Version of the addressbook")

;;;; * Customization

(defgroup addrbook nil
  "Addressbook subsytem"
  :group 'applications
  :link '(url-link "http://www.emacswiki.org/cgi-bin/wiki/AddressBook"))

(defgroup addrbook-hooks nil
  "Addressbook hooks"
  :group 'addrbook)

(defcustom addrbook-directory "~/.contacts"
  "Directory with stored vCards"
  :type 'directory
  :group 'addrbook)

(defcustom addrbook-display-images t
  "Display images in the addressbook"
  :type 'boolean
  :group 'addrbook)

(defcustom addrbook-display-groups
  '(identification-properties)
  "Groups to expand by default"
  :type 'sexp
  :group 'addrbook)

(defcustom addrbook-force-addressbook-creation
  t
  "Force the creation of the addressbook file if it doesnt exist upon startup"
  :type 'boolean
  :group 'addrbook)

(defcustom addrbook-attribute-indentation
  1
  "Indentation deep for attribute titles"
  :type 'integer
  :group 'addrbook)

(defcustom addrbook-field-indentation
  2
  "Indentation deep for attribute fields"
  :type 'integer
  :group 'addrbook)

(defcustom addrbook-ask-for-search
  nil
  "Whether the addressbook should ask for a search upon `addressbook' invocation"
  :type 'boolean
  :group 'addrbook)

(defcustom addrbook-field-for-sort
  "First name"
  "Field to use when sorting contacts.

It may be \"Surname\", \"First Name\", \"AKA\", \"Name prefix\" or \"Name suffix\"."
  :type 'string
  :group 'addrbook)

(defcustom addrbook-use-multiple-frames
  nil
  "If t, open new frames when switching summary<->contact"
  :type 'boolean
  :group 'addrbook)

(defface addrbook-summary-card-number
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
  :group 'addrbook)

(defface addrbook-summary-modified-flag
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
  :group 'addrbook)

(defface addrbook-summary-match-flag
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
  :group 'addrbook)

(defface addrbook-properties-group-name
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
  :group 'addrbook)

(defface addrbook-attribute-title-name
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
  :group 'addrbook)

(defface addrbook-contact-title
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
  :group 'addrbook)

(defface addrbook-attribute-type
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
  :group 'addrbook)

(defface addrbook-attribute-value
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
  :group 'addrbook)

(defface addrbook-summary-selected-card
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
  :group 'addrbook)

;;;; * Variables

(defvar addrbook-image-types
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

(defvar addrbook-cards nil
  "Cards of the current addressbook")

(defvar addrbook-modified-cards nil
  "Indexes of modified cards in addrbook-cards")

(defvar addrbook-current-card nil
  "Number of current card")

(defvar addrbook-properties
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

(defvar addrbook-required-attrs '("n")
  "List of required attributes")

(defvar addrbook-general-params
  '(("url" "value") ("content-id" "value"))
  "General vCard parameters")

;;;; * Properties management functions

;;;; ** Groups
(defun addrbook-get-group (group-symbol)
  "Return the sexp containing information for GROUP"
  (assoc group-symbol addrbook-properties))

(defun addrbook-get-group-symbol (group)
  (nth 0 group))

(defun addrbook-get-group-name (group)
  (nth 1 group))

(defun addrbook-get-group-props (group)
  (nth 2 group))

(defun addrbook-get-group-letter (group)
  (nth 3 group))

(defun addrbook-group-has-properties-p (group)
  (let ((group-attrs (addrbook-get-group-props group))
        (result nil))
    (dolist (attr (addrbook-get-card addrbook-current-card))
      (if (and (addrbook-property-in-group-p attr group-attrs)
               (not (member (vcard-attr-get-name attr) addrbook-contact-properties-nodisplay)))
          (setq result t)))
    result))

;;;; ** Properties

(defun addrbook-get-group-prop (props prop-name)
  (assoc prop-name props))

(defun addrbook-get-prop-name (property)
  (nth 0 property))

(defun addrbook-get-prop-title (property)
  (nth 1 property))

(defun addrbook-get-prop-letter (property)
  (nth 2 property))

(defun addrbook-get-prop-fields-list (property)
  (nth 3 property))

(defun addrbook-get-prop-fields (property)
  (let ((fields (addrbook-get-prop-fields-list property))
        (result nil) field)
    (dolist (field fields)
      (setq result (cons (car field) result)))
    (reverse result)))

(defun addrbook-get-prop-field-name (field)
  (nth 0 field))

(defun addrbook-get-prop-field-letter (field)
  (nth 1 field))

(defun addrbook-get-prop-index (prop-fields field-name)
  (let ((index 0) result)
    (dotimes (index (length prop-fields) result)
      (if (equal (nth index prop-fields) field-name)
          (setq result index)))))

(defun addrbook-get-prop-field-description (fields field-name)
  (cadr (assoc field-name fields)))

(defun addrbook-get-prop-parameters (prop)
  (nthcdr 4 prop))

(defun addrbook-get-prop-parameter (prop param-name)
  (let ((prop-parameters (addrbook-get-prop-parameters prop)))
    (cadr (assoc param-name prop-parameters))))

(defun addrbook-prop-parameter-allow-duplicates (prop param-name)
  (let ((prop-parameters (addrbook-get-prop-parameters prop)))
    (nth 2 (assoc param-name prop-parameters))))

(defun addrbook-prop-parameter-is-mandatory (prop param-name)
  (let ((prop-parameters (addrbook-get-prop-parameters prop)))
    (nth 3 (assoc param-name prop-parameters))))

(defun addrbook-property-in-group-p (attr group-props)
  (let ((attr-name (vcard-attr-get-name attr)))
    (when (assoc attr-name group-props)
      t)))

(defun addrbook-get-property (attr-name)
  (let (group result)
    (dolist (group addrbook-properties)
      (let* ((group-props (addrbook-get-group-props group))
             (group-prop (addrbook-get-group-prop group-props attr-name)))
        (if group-prop
            (setq result group-prop))))
    result))

(defun addrbook-get-prop-default-type (prop-name)
  (let* ((property (addrbook-get-property prop-name))
         (prop-type (addrbook-get-prop-parameter property "type")))
    (when prop-type
      (car (car prop-type)))))

;;;; ** Cards

(defun addrbook-get-card (numcard)
  (nth numcard addrbook-cards))

(defun addrbook-set-card (numcard card)
  (if addrbook-cards
      (cond
       ((and (>= numcard 0) (< numcard (length addrbook-cards)))
        (setcar (nthcdr numcard addrbook-cards) card))
       ((>= numcard (length addrbook-cards))
        (setq addrbook-cards (append addrbook-cards (list card)))))
    (setq addrbook-cards (list card))))

(defun addrbook-remove-card (numcard)
  (setq addrbook-cards (delete (addrbook-get-card numcard)
                               addrbook-cards)))

(defun addrbook-value-empty-p (values)
  "Return t if VALUES is empty"
  (when (listp values)
    (if (cdr values)
        (when (equal (car values) "")
          (addrbook-value-empty-p (cdr values)))
      (equal (car values) ""))))

(defun addrbook-number-of-values (values)
  (if (listp values)
      (let (value
            (nov 0))
        (dolist (value values nov)
          (if (not (equal value ""))
              (setq nov (+ nov 1)))))
    1))

(defun addrbook-get-card-fn (&optional with-aka card-number)
  (let* ((card (addrbook-get-card (if card-number
                                      card-number
                                    addrbook-current-card)))
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

;;;; ** Attributes

(defun addrbook-delete-attr (attr-index attr-subindex)
  (let* ((card (addrbook-get-card addrbook-current-card))
         (attr (vcard-get-attribute card attr-index))
         (attr-value (vcard-attr-get-values attr)))
    (if attr-subindex
        (progn
          ;; Delete the field from the values
          (setcar (nthcdr attr-subindex attr-value) "")
          (vcard-attr-set-values attr attr-value)
          (if (addrbook-value-empty-p attr-value)
              (addrbook-set-card addrbook-current-card (vcard-delete-indexed-attribute card attr-index))))
      ;; Delete the attribute
      (addrbook-set-card addrbook-current-card (vcard-delete-indexed-attribute card attr-index)))))

(defun addrbook-build-custom-property-group ()
  "Return an empty custom property group"
  (list 'custom-properties
        "Custom Properties"
        nil
        ?c))

(defun addrbook-set-custom-properties (props-data)
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
    (when (not (addrbook-get-group 'custom-properties))
      (setq addrbook-properties
            (append addrbook-properties (list (addrbook-build-custom-property-group)))))
    (setq custom-group (addrbook-get-group 'custom-properties))
    (setcar (nthcdr 2 custom-group) props-data)))


;;;; * Addressbook contact editor

;;;; ** Constants

(defconst addrbook-contact-buffer-name "*AddressBook Contact*"
  "Name of the buffer for the addressbook contact editor")

;;;; ** Variables

(defvar addrbook-contact-properties-nodisplay
  '("sound" "agent" "version" "uid" "label" "mailer" "uid"))

(defvar addrbook-contact-mode-map nil
  "Keymap for addrbook-contact-mode")

(defvar addrbook-contact-displayed-groups nil
  "List of displayed property groups")

(defvar addrbook-contact-mode-line-string " ABook Contact"
  "String to display on the mode line when in the addressbook mode.
If `nil', do not show anything.")

;;;; ** Contact buffer management

(defun addrbook-create-contact-buffer ()
  "Create a new addressbook buffer to show contact information"
  (setq buffer (get-buffer-create addrbook-contact-buffer-name))
  (set-buffer buffer)
  (addrbook-contact-mode))

(defun addrbook-show-contact ()
  (let ((buffer (get-buffer addrbook-contact-buffer-name)))
    (if addrbook-use-multiple-frames
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))))


;;;; ** Commands

(defun addrbook-contact-add-attribute-type ()
  "Add a new type to the attribute under point"
  (interactive)
  (let ((buffer-read-only nil)
        (point-backup (point))
        (group-symbol (addrbook-contact-get-current-group))
        (attr-index (addrbook-contact-get-current-attr-index))
        (attr-subindex (addrbook-contact-get-current-attr-subindex)))
    (if (and attr-index
             (not attr-subindex))
        (let* ((card (addrbook-get-card addrbook-current-card))
               (attr (vcard-get-attribute card attr-index))
               (attr-name (vcard-attr-get-name attr))
               (property (addrbook-get-property attr-name))
               (prop-types (addrbook-get-prop-parameter property "type")))
          (if prop-types
              (let ((new-type (addrbook-select-non-existing-type attr))
                    type result)
                (dolist (type prop-types)
                  (if (equal (cadr type)
                             new-type)
                      (setq result (car type))))
                (when result
                  (if (addrbook-prop-parameter-allow-duplicates property "type")
                      ;; Add the new type
                      (vcard-attr-add-property attr "type" result)
                    ;; Replace current type
                    (vcard-attr-set-property attr "type" result))
                  ;; Redisplay attribute
                  (addrbook-contact-redisplay-attr-at-point)
                  ;; Addressbook modified
                  (add-to-list 'addrbook-modified-cards addrbook-current-card))))))
    (goto-char point-backup)))

(defun addrbook-contact-remove-attribute-type ()
  "Remove a type from the attribute under point"
  (interactive)
  (let ((buffer-read-only nil)
        (point-backup (point))
        (group-symbol (addrbook-contact-get-current-group))
        (attr-index (addrbook-contact-get-current-attr-index))
        (attr-subindex (addrbook-contact-get-current-attr-subindex)))
    (if (and attr-index
             (not attr-subindex))
        (let* ((card (addrbook-get-card addrbook-current-card))
               (attr (vcard-get-attribute card attr-index))
               (attr-name (vcard-attr-get-name attr))
               (property (addrbook-get-property attr-name))
               (prop-types (addrbook-get-prop-parameter property "type")))
          (if prop-types
              (if (and (equal (length (vcard-attr-get-parameter attr "type")) 1)
                       (addrbook-prop-parameter-is-mandatory property "type"))
                  (message "This attribute should have a type")
                (let ((new-type (addrbook-select-existing-type attr))
                      type result)
                  (dolist (type prop-types)
                    (if (equal (cadr type)
                               new-type)
                        (setq result (car type))))
                  (when result
                    ;; Add the new type
                    (vcard-attr-remove-property attr "type" result)
                    ;; Redisplay attribute
                    (addrbook-contact-redisplay-attr-at-point)
                    ;; Addressbook modified
                    (add-to-list 'addrbook-modified-cards addrbook-current-card)))))))
    (goto-char point-backup)))

(defun addrbook-contact-delete-attribute ()
  "Delete the attribute under point"
  (interactive)
  (let ((buffer-read-only nil)
        (point-backup (point))
        (group-symbol (addrbook-contact-get-current-group))
        (attr-index (addrbook-contact-get-current-attr-index))
        (attr-subindex (addrbook-contact-get-current-attr-subindex)))
  (if (and group-symbol attr-index)
      (let* ((group (addrbook-get-group group-symbol))
             (group-attrs (addrbook-get-group-props group))
             (card (addrbook-get-card addrbook-current-card))
             (attr (vcard-get-attribute card attr-index))
             (attr-name (vcard-attr-get-name attr))
             (attr-value (vcard-attr-get-values attr))
             (group-attr (addrbook-get-group-prop group-attrs attr-name))
             (attr-title (addrbook-get-prop-title group-attr))
             (attr-fields (addrbook-get-prop-fields-list group-attr))
             (attr-field (when attr-subindex (nth attr-subindex attr-fields)))
             (attr-field-name (when attr-field (addrbook-get-prop-field-name attr-field)))
             (prompt (concat "Are you sure you want to delete "
                             (if attr-subindex
                                 (concat "field " attr-field-name)
                               (concat "attribute " attr-title))
                             "? "))
             elt)
        (if (yes-or-no-p prompt)
            (if (and (member attr-name addrbook-required-attrs)
                     (or (addrbook-contact-get-current-attr-compound-title)
                         (equal (addrbook-number-of-values attr-value) 1)))
                (error "Trying to delete a required attribute")
              (addrbook-delete-attr attr-index attr-subindex)
              (if (not (equal attr-name "photo"))
                  (addrbook-contact-redisplay-group group-symbol)
                (addrbook-contact-redisplay-card))))))
  (goto-char point-backup)
  (add-to-list 'addrbook-modified-cards addrbook-current-card)))

(defun addrbook-contact-add-attribute ()
  "Add a new attribute to the current card"
  (interactive)
  (let* (buffer-read-only
         (backup-point (point))
         group-symbol
         group group-attrs
         (i 0)
         (current-card (addrbook-get-card addrbook-current-card)))
    ;; Get group
    (setq group-symbol (or (addrbook-contact-get-current-group)
                           (addrbook-select-group)))
    (setq group (addrbook-get-group group-symbol))
    (setq group-attrs (addrbook-get-group-props group))
    (if group-symbol
        (let (attr-index attr-subindex property-index)
          ;; Get property
          (setq attr-index (addrbook-contact-get-current-attr-index))
          (setq attr-subindex (addrbook-contact-get-current-attr-subindex))
          (if (and attr-index attr-subindex)
              (let ((attr (vcard-get-attribute current-card attr-index)))
                (setq property-name (vcard-attr-get-name attr)))
            (setq property-name (addrbook-select-property group-symbol)))
          (if property-name
              (let* ((property (addrbook-get-group-prop group-attrs property-name))
                     (property-title (addrbook-get-prop-title property))
                     (property-fields (addrbook-get-prop-fields property))
                     field-index (property-value "") prompt
                     (continue t))
                ;; Get field
                (when property-fields
                  (setq field-index (addrbook-select-field group-symbol property-name))
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
                    (let* ((new-attr-type (addrbook-get-prop-default-type property-name))
                           (new-attr-name property-name)
                           (new-attr-values property-value)
                           new-attr)
                      (setq new-attr (list (list new-attr-name)
                                           new-attr-values))
                      (vcard-attr-set-property new-attr "type" new-attr-type)
                      (if (equal new-attr-name "photo")
                          (vcard-attr-set-property new-attr "value" "url"))
                      (setq current-card (vcard-add-attribute current-card new-attr))))
                  (addrbook-set-card addrbook-current-card current-card)
                  (if (addrbook-contact-in-display-p group-symbol)
                      (progn
                        ;; Redisplay the group with new contents
                        (addrbook-contact-redisplay-group group-symbol)
                        ;; Hide the group if it was hidden
                        (if (addrbook-contact-group-hidden-p group)
                            (addrbook-contact-hide-show-group group nil)))
                    ;; Redisplay the entire card
                    (addrbook-contact-redisplay-card))
                  ;; This card has been modified
                  (add-to-list 'addrbook-modified-cards addrbook-current-card))))))
    (goto-char backup-point)))

(defun addrbook-contact-edit-attribute ()
  (interactive)
  "Edit the value of the attribute located in the current line"
  (let ((buffer-read-only nil)
        (group-symbol (addrbook-contact-get-current-group))
        (attr-index (addrbook-contact-get-current-attr-index))
        (attr-subindex (addrbook-contact-get-current-attr-subindex))
        (attr-compound-title-p (addrbook-contact-get-current-attr-compound-title)))
    (if (and group-symbol attr-index (not attr-compound-title-p))
        (let* ((group (addrbook-get-group group-symbol))
               (group-attrs (addrbook-get-group-props group))
               (card (addrbook-get-card addrbook-current-card))
               (attr (vcard-get-attribute card attr-index))
               (attr-name (vcard-attr-get-name attr))
               (attr-value (vcard-attr-get-values attr))
               (group-attr (addrbook-get-group-prop group-attrs attr-name))
               (attr-fields (addrbook-get-prop-fields group-attr))
               (attr-real-value (if attr-subindex
                                    (nth attr-subindex attr-value)
                                  (car attr-value)))
               (attr-real-name (if attr-subindex
                                   (nth attr-subindex attr-fields)
                                 (addrbook-get-prop-title group-attr)))
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
          ;; FIXME: use addrbook-contact-redisplay-attr-at-point
          (let ((column-backup (current-column))
                (line-backup (line-number-at-pos (point))))
            (addrbook-contact-erase-attr-region)
            (addrbook-contact-display-attribute attr-index)
            (goto-line line-backup)
            (goto-char (+ (line-beginning-position) column-backup)))))))

(defun addrbook-contact-goto-next-group ()
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

(defun addrbook-contact-toggle-hide-show-group ()
  "When staying on a parameters group title, toggle visibility of the group"
  (interactive)
  (let ((buffer-read-only nil)
        (group (get-text-property (point) 'group))
        (group-content-pos nil))
    (when group
      ;; Search for visibility properties in group contents
      (setq group-content-pos (next-single-property-change (point) 'group))
      (if (get-text-property group-content-pos 'invisible)
          (addrbook-contact-hide-show-group group t)
        (addrbook-contact-hide-show-group group nil)))))

(defun addrbook-contact-hide-all-groups ()
  "Hide all displayed groups"
  (interactive)
  (let (buffer-read-only)
    (dolist (group addrbook-contact-displayed-groups nil)
      (addrbook-contact-hide-show-group group nil))))

(defun addrbook-contact-show-all-groups ()
  "Show all displayed groups"
  (interactive)
  (let (buffer-read-only)
    (dolist (group addrbook-contact-displayed-groups nil)
      (addrbook-contact-hide-show-group group t))))

(defun addrbook-contact-cycle-groups ()
  "Cycle to next group"
  (interactive)
  (let ((next-group-pos (addrbook-contact-goto-next-group)))
    (when (not next-group-pos)
      (goto-char (point-min))
      (addrbook-contact-goto-next-group))))

;;;; ** Modeline management

(defun addrbook-contact-set-mode-line (card-number total-cards)
  "Update the modeline of the current buffer"
  ;; FIXME: this is ugly
  (when addrbook-contact-mode-line-string
    (setq mode-line-buffer-identification
	  (list 24
              addrbook-contact-mode-line-string
              ": "
              (list 10
                    (format "%d/%d" card-number total-cards))))))

;;;; ** Major mode
(defun addrbook-contact-mode ()
      "A major mode for contact editing

Commands:
\\{addrbook-contact-mode-map}"
      (interactive)
      (kill-all-local-variables)
      (setq addrbook-contact-mode-map (make-keymap))
      (define-key addrbook-contact-mode-map "c" 'addrbook-create-card)
      (define-key addrbook-contact-mode-map "D" 'addrbook-delete-card)
      (define-key addrbook-contact-mode-map "n" 'addrbook-next-contact)
      (define-key addrbook-contact-mode-map "p" 'addrbook-previous-contact)
      (define-key addrbook-contact-mode-map "s" 'addrbook-save-cards)
      (define-key addrbook-contact-mode-map "x" 'addrbook-export-card)
      (define-key addrbook-contact-mode-map "b" 'addrbook-bury)
      (define-key addrbook-contact-mode-map "q" 'addrbook-quit)
      (define-key addrbook-contact-mode-map "e" 'addrbook-contact-edit-attribute)
      (define-key addrbook-contact-mode-map (kbd "SPC") 'addrbook-contact-toggle-hide-show-group)
      (define-key addrbook-contact-mode-map (kbd "TAB") 'addrbook-contact-cycle-groups)
      (define-key addrbook-contact-mode-map "d" 'addrbook-contact-delete-attribute)
      (define-key addrbook-contact-mode-map "a" 'addrbook-contact-add-attribute)
      (define-key addrbook-contact-mode-map "t" 'addrbook-contact-add-attribute-type)
      (define-key addrbook-contact-mode-map "r" 'addrbook-contact-remove-attribute-type)
      (define-key addrbook-contact-mode-map "m" 'addrbook-send-email)
      (define-key addrbook-contact-mode-map "H" 'addrbook-contact-hide-all-groups)
      (define-key addrbook-contact-mode-map "S" 'addrbook-contact-show-all-groups)
      (define-key addrbook-contact-mode-map "h" 'addrbook-summarize)
      (use-local-map addrbook-contact-mode-map)
      (setq mode-name "ABook Contact")
      (setq major-mode 'addrbook-contact-mode))

;;;; * Addressbook Summary

;;;; ** Constants

(defconst addrbook-summary-buffer-name "*AddressBook Summary*"
  "Name of the buffer for the addressbook summary")

;;;; ** Variables

(defvar addrbook-summary-mode-map nil
  "Keymap for addrbook-summary-mode")

(defvar addrbook-summary-mode-line-string " ABook Summary"
  "String to display on the mode line when in the addressbook summary mode.
If `nil', do not show anything.")

;;;; ** Summary buffer management

(defun addrbook-make-summary-buffer ()
  (save-excursion
    (let ((buffer (get-buffer-create addrbook-summary-buffer-name)))
      (set-buffer buffer)
      (addrbook-summary-mode)
      (addrbook-summary-display)
      (setq buffer-read-only t)
      (setq addrbook-summary-buffer buffer)
      buffer)))

(defun addrbook-summary ()
  "Open the addressbook and show the summary window"
  (let ((buffer (get-buffer addrbook-summary-buffer-name)))
    (when (not buffer)
      (setq buffer (addrbook-make-summary-buffer)))
    (switch-to-buffer-other-window addrbook-summary-buffer)
    (addrbook-summary-goto-contact 0 t)))

(defun addrbook-summarize ()
  "Summarize the contents of the addressbook in a summary buffer.

The format is as described in the variable `addrbook-summary-format'"
  (interactive)
  (if (not (get-buffer addrbook-summary-buffer-name))
      (save-excursion
        (addrbook-get-create-summary-buffer)
        (set-buffer (get-buffer addrbook-summary-buffer-name))
        (addrbook-summary-goto-contact addrbook-current-card nil)))
  (addrbook-show-summary))

(defun addrbook-show-summary ()
  (let ((buffer (get-buffer addrbook-summary-buffer-name)))
    (if addrbook-use-multiple-frames
        (switch-to-buffer-other-window buffer)
      (switch-to-buffer buffer))
    (addrbook-summary-goto-contact addrbook-current-card nil)))

(defun addrbook-get-create-summary-buffer ()
  (if (not addrbook-summary-buffer)
      (save-excursion
        (setq addrbook-summary-buffer (get-buffer-create addrbook-summary-buffer-name))
        (set-buffer addrbook-summary-buffer)
        (addrbook-summary-mode)
        (addrbook-summary-display)))
  addrbook-summary-buffer)

;;;; ** Display functions

(defun addrbook-summary-display ()
  (erase-buffer)
  (let (card-index card name)
    (dotimes (card-index (length addrbook-cards))
      (insert "  ")
      (insert " ")
      (insert (propertize (number-to-string (+ card-index 1))
                          'face 'addrbook-summary-card-number)
              " ")
      (insert (make-string (- 4 (length (number-to-string card-index))) ?\ ))
      (setq card (addrbook-get-card card-index))
      (setq name (vcard-get-named-attribute card "n"))
      (insert (propertize (addrbook-get-card-fn t card-index)
                          'face 'addrbook-attribute-value))
      (add-text-properties (line-beginning-position)
                           (line-end-position)
                           (list 'card-index card-index))
      (insert (propertize "\n"
                          'card-index card-index)))))

(defun addrbook-summary-goto-contact (numcard update-contact-buffer)
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
        (overlay-put highlight-overlay 'face 'addrbook-summary-selected-card))
      (addrbook-summary-set-mode-line (+ numcard 1) (length addrbook-cards))
      (when (and update-contact-buffer
                 (get-buffer addrbook-contact-buffer-name))
        (save-excursion
          (set-buffer (get-buffer addrbook-contact-buffer-name))
          (let (buffer-read-only)
            (addrbook-contact-display-card numcard)))))))

(defun addrbook-summary-get-current-card ()
  (get-text-property (point) 'card-index))

;;;; ** Commands

(defun addrbook-summary-next-contact ()
  "Select the next card in the summary buffer"
  (interactive)
  (let ((card-index (addrbook-summary-get-current-card)))
    (cond
     ((equal card-index (- (length addrbook-cards) 1))
      (addrbook-summary-goto-contact 0 t))
     (t
      (addrbook-summary-goto-contact (+ card-index 1) t)))))

(defun addrbook-summary-previous-contact ()
  "Select the previous card in the summary buffer"
  (interactive)
  (let ((card-index (addrbook-summary-get-current-card)))
    (cond
     ((equal card-index 0)
      (addrbook-summary-goto-contact (- (length addrbook-cards) 1) t))
     (t
      (addrbook-summary-goto-contact (- card-index 1) t)))))

(defun addrbook-summary-show-contact ()
  "Open an addressbook buffer to show the current selected card"
  (interactive)
  (let ((card-index (addrbook-summary-get-current-card)))
    (when (not (get-buffer addrbook-contact-buffer-name))
      (save-excursion
        (addrbook-create-contact-buffer)
        (addrbook-contact-display-card card-index)
        (setq buffer-read-only t)))
    (addrbook-show-contact)))

;; Modeline management

(defun addrbook-summary-set-mode-line (card-number total-cards)
  "Update the mdoeline of the current summary buffer"
  ;; FIXME: this is ugly
  (when addrbook-summary-mode-line-string
    (setq mode-line-buffer-identification
	  (list 24
		addrbook-summary-mode-line-string
		": "
		(list 10
		      (format "%d/%d" card-number total-cards))))))

;;;; ** Major mode

(defun addrbook-summary-mode ()
  "A major mode for the addressbook summary window

Commands:
\\{addrbook-summary-mode-map}"
  (interactive)
  (kill-all-local-variables)
  (setq addrbook-summary-mode-map (make-keymap))
  (define-key addrbook-summary-mode-map "n" 'addrbook-summary-next-contact)
  (define-key addrbook-summary-mode-map "p" 'addrbook-summary-previous-contact)
  (define-key addrbook-summary-mode-map (kbd "<down>") 'addrbook-summary-next-contact)
  (define-key addrbook-summary-mode-map (kbd "<up>") 'addrbook-summary-previous-contact)
  (define-key addrbook-summary-mode-map (kbd "RET") 'addrbook-summary-show-contact)
  (define-key addrbook-summary-mode-map "b" 'addrbook-bury)
  (define-key addrbook-summary-mode-map "q" 'addrbook-quit)
  (define-key addrbook-summary-mode-map "a" 'addrbook-create-card)
  (define-key addrbook-summary-mode-map "i" 'addrbook-import-vcard)
  (define-key addrbook-summary-mode-map "x" 'addrbook-export-vcard)
  (define-key addrbook-summary-mode-map "m" 'addrbook-send-email)
  (use-local-map addrbook-summary-mode-map)
  (setq mode-name "AddressBook Summary")
  (setq major-mode 'addrbook-summary-mode))

;;;; * General commands (usable from all addressbook modes)

(defun addrbook-send-email ()
  "Send an email to current contact"
  (interactive)
  (let* ((card (addrbook-get-card addrbook-current-card))
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
        (setq letter (addrbook-fast-selection mail-names "Select email address to send mail to"))
        (dolist (name mail-names)
          (if (equal letter
                     (cadr name))
              (setq sendto-address (car name)))))
      ;; Send the email
      (if sendto-address
          (compose-mail-other-frame (concat
                                     "\"" (addrbook-get-card-fn) "\""
                                     " <" sendto-address ">"))))))

(defun addrbook-delete-card ()
  "Delete the current card"
  (interactive)
  (let ((buffer-read-only nil)
        (current-card addrbook-current-card)
        (prompt "Are you sure you want to delete current contact? "))
    (when (yes-or-no-p prompt)
      (if (equal current-card (- (length addrbook-cards) 1))
          (setq current-card (- (length addrbook-cards) 2)))
      (addrbook-remove-card addrbook-current-card)
      (add-to-list 'addrbook-modified-cards current-card)
      (if (equal (length addrbook-cards) 0)
          (addrbook-quit)
        (addrbook-contact-display-card current-card)))))

(defun addrbook-create-card ()
  "Create a new card"
  (interactive)
  (let ((buffer-read-only nil)
        (new-card-index (addrbook-create-card-2)))
    (if new-card-index
        (addrbook-contact-display-card new-card-index))))

(defun addrbook-create-card-2 ()
  "Create a new card with minimum identification properties and insert it
into `addrbook-cards'.

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
         (new-card-index (length addrbook-cards)))
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
      (addrbook-set-card new-card-index new-card)
      (add-to-list 'addrbook-modified-cards new-card-index)
      new-card-index)))

(defun addrbook-import-vcard (filename)
  "Import vCard from FILENAME and add it into our contact database and return index card number."
  (interactive
   (list
    (expand-file-name
     (read-file-name "vCard file to import: "))))

  (let ((index nil)
	vcard)
    (addrbook-be-read-cards)
    (save-excursion
      (unwind-protect
	  (if (and (setq index (length addrbook-cards))
		   (setq vcard (vcard-parse-file filename)))
	      (progn
		(addrbook-set-card index (car vcard))
		(add-to-list 'addrbook-modified-cards index))
	    (error "Vcard import failed!"))
	;; Just to be sure, call save-cards
	(addrbook-save-cards nil)))
    index))

;; FIXME: does not work in contact mode
(defun addrbook-export-vcard ()
  "Export current card data to a file."
  (interactive)
  (let* ((index (addrbook-summary-get-current-card))
	 (fullname (addrbook-get-card-fn nil index ))
	 (filename (read-file-name "Export vCard to file: " nil nil
				   nil (concat fullname ".vcf"))))
    (addrbook-write-data-1 filename (addrbook-get-card index))
    (message "vCard exported")))

(defun addrbook-write-data-1 (filename &optional vcard)
  "Save raw vCard formatted data into FILENAME.
If optional VCARD parameter is not set, use `addrbook-current-card'."
  (let ((vcard (or vcard (addrbook-get-card addrbook-current-card))))
    (with-temp-file filename
      (vcard-insert vcard))))

(defun addrbook-send-email ()
  "Send an email to current contact"
  (interactive)
  (let* ((card (addrbook-get-card addrbook-current-card))
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
        (setq letter (addrbook-fast-selection mail-names "Select email address to send mail to"))
        (dolist (name mail-names)
          (if (equal letter
                     (cadr name))
              (setq sendto-address (car name)))))
      ;; Send the email
      (if sendto-address
          (compose-mail-other-frame (concat
                                     "\"" (addrbook-get-card-fn) "\""
                                     " <" sendto-address ">"))))))

(defun addrbook-save-cards (prefix)
  "Save cards into addrbook-file"
  (interactive "P")
  (if prefix
      (addrbook-export-card)
    ;; Save modified cards into addressbook-file
    (if (equal (length addrbook-modified-cards) 0)
        (message "addressbook not saved")
      (let ((i 0))
        (dotimes (i (length addrbook-cards))
          (when (member i addrbook-modified-cards)
            (addrbook-be-write-card i))))
      (setq addrbook-modified-cards nil)
      (set-buffer-modified-p nil)
      (message "addressbook saved"))))

(defun addrbook-next-contact ()
  "Display the next card"
  (interactive)
  (let (buffer-read-only window-list win)
    (if (equal addrbook-current-card (- (length addrbook-cards) 1))
        (message "No more cards")
      (addrbook-contact-display-card (+ addrbook-current-card 1))
      (let ((summary-buffer (get-buffer addrbook-summary-buffer)))
        (when summary-buffer
          (setq window-list (get-buffer-window-list summary-buffer nil t))
          (dolist (win window-list)
            (with-selected-window (get-buffer-window summary-buffer t)
              (addrbook-summary-goto-contact addrbook-current-card nil))))))))

(defun addrbook-previous-contact ()
  "Display the previous card"
  (interactive)
  (let (buffer-read-only)
    (if (equal addrbook-current-card 0)
        (message "First card")
      (addrbook-contact-display-card (- addrbook-current-card 1))
      (let ((summary-buffer (get-buffer addrbook-summary-buffer)))
        (when summary-buffer
          (setq window-list (get-buffer-window-list summary-buffer nil t))
          (dolist (win window-list)
            (with-selected-window (get-buffer-window summary-buffer t)
              (addrbook-summary-goto-contact addrbook-current-card nil))))))))

(defun addrbook-quit ()
  "Exit the addressbook."
  (interactive)
  (if (and (not (equal (length addrbook-modified-cards) 0))
           (yes-or-no-p "Save addressbook? "))
      (addrbook-save-cards nil))
  (let ((contact-buffer (get-buffer addrbook-contact-buffer-name))
        (summary-buffer (get-buffer addrbook-summary-buffer-name))
        win window-list)
    (when summary-buffer
      ;; Delete windows (and possibly frames)
      (delete-windows-on summary-buffer)
      (kill-buffer summary-buffer))
    (when contact-buffer
      ;; Delete windows (and possibly frames)
      (delete-windows-on contact-buffer)
      (kill-buffer contact-buffer))))

(defun addrbook-bury ()
  "Bury the addressbook buffer(s)."
  (interactive)
  (when (or (eq major-mode 'addrbook-summary-mode)
	    (eq major-mode 'addrbook-contact-mode ))
    (bury-buffer)))

(defun addrbook-export-card ()
  "Export current card data to a file"
  (interactive)
  (let ((filename (read-file-name "Export vCard to file: "))
        (card (addrbook-get-card addrbook-current-card)))
    (with-temp-file filename
      (vcard-insert card))
    (message "vCard exported")))

;;;; * Backend management

;;;; ** Customization and Variables

(defcustom addrbook-backend
  'addrbook-backend-simple
  "Backend to use for the addressbook. 

Currently there are two backends available: `addrbook-backend-simple' (simple backend
to store all contacts in one file) and `addrbook-backend-multiple' (that stores one contact per file in
a given directory"
  :type 'symbol)

;;;; ** Utility functions

(defun addrbook-make-params-explicit ()
  "Make unambiguous anonymous params explicit.

It uses `addrbook-general-params' and the type parameter for each property
defined in `addrbook-properties'"
  (let ((i 0))
    (dolist (card addrbook-cards)
      (dotimes (i (vcard-get-num-attributes card))
        (let* ((attr (vcard-get-attribute card i))
               (attr-name (vcard-attr-get-name attr))
               (attr-props (cdr (vcard-attr-get-proplist attr)))
               (property (addrbook-get-property attr-name))
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
                  (let* ((general-param (assoc param-value addrbook-general-params))
                         (general-param-name (if general-param (cadr general-param)))
                         (prop-types (addrbook-get-prop-parameter property "type")))
                    (if general-param-name
                        (setq param-name general-param-name)
                      (if (and prop-types
                               (assoc param-value prop-types))
                          (setq param-name "type")))
                    (if param-name
                        (setcar (nthcdr j attr-props) (cons param-name param-value))))))))))))

;;;; ** API

(defun addrbook-be-read-cards ()
  "Read cards from an addressbook backend.

This function stores the retrieved vCard information in 
`addrbook-cards'."
  (cond
   ((equal addrbook-backend 'addrbook-backend-simple)
    (addrbook-be-simple-read-cards))
   ((equal addrbook-backend 'addrbook-backend-multiple)
    (addrbook-be-multiple-read-cards))
   (t
    (error "No valid addressbook backend selected.")))
  (when addrbook-cards
    (addrbook-make-params-explicit)
    t))

(defun addrbook-be-write-card (card-id)
  "Write the CARD-ID card to the appropiate backend."
  (cond
   ((equal addrbook-backend 'addrbook-backend-simple)
    (addrbook-be-simple-write-card card-id))
   ((equal addrbook-backend 'addrbook-backend-multiple)
    (addrbook-be-multiple-write-card card-id))
   (t
    (error "No valid addressbook backend selected."))))

(defun addrbook-be-delete-card (card-id)
  "Delete the CARD-ID card from the appropiate backend."
  (cond
   ((equal addrbook-backend 'addrbook-backend-simple)
    (addrbook-be-simple-delete-card card-id))
   ((equal addrbook-backend 'addrbook-backend-multiple)
    (addrbook-be-multiple-delete-card card-id))
   (t
    (error "No valid addressbook backend selected."))))

(defun addrbook-be-card-modified (card-id)
  "Tell the backend a given card has been modified"
  (cond
   ((equal addrbook-backend 'addrbook-backend-simple)
    (addrbook-be-simple-card-modified card-id))
   ((equal addrbook-backend 'addrbook-backend-multiple)
    (addrbook-be-multiple-card-modified card-id))
   (t
    (error "No valid addressbookº  backend selected."))))
  
;;;; ** Simple backend

(defcustom addrbook-file "~/.addressbook"
  "File with stored addresses"
  :type 'file
  :group 'addrbook)

(defun addrbook-be-simple-read-cards ()
  "Read cards from addressbook file"
  (with-temp-buffer
    (insert-file-contents addrbook-file)
    (setq addrbook-cards (vcard-parse-region (point-min)
                                             (point-max)))))

(defun addrbook-be-simple-write-card (card-id)
  "Write cards information to `addrbook-file', discarding any
previous content."
  (with-temp-file addrbook-file
    (dotimes (i (length addrbook-cards))
      (let ((card (addrbook-get-card i)))
        (vcard-insert card)
        (if (not (equal i (- (length addrbook-cards) 1)))
            (insert "\n\n"))))))

;;;; * Utility functions

(defun addrbook-list-to-csv (list)
  (let ((result "")
        i)
    (dotimes (i (length list))
      (setq result (concat result (nth i list)))
      (if (not (equal i (- (length list) 1)))
          (setq result (concat result ","))))
    result))

(defun addrbook-open ()
  "Open the addressbook"
  (or (addrbook-be-read-cards)
      (addrbook-create-card-2)))

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

(defun addrbook-sort-cards ()
  "Sort `addrbook-cards' using the `addrbook-field-for-sort' field"
  (setq addrbook-cards
        (sort addrbook-cards
              (lambda (card1 card2)
                (let* ((card1-n (vcard-get-named-attribute card1 "n"))
                       (card2-n (vcard-get-named-attribute card2 "n"))
                       (n-prop (addrbook-get-property "n"))
                       (n-fields (addrbook-get-prop-fields n-prop))
                       (field-index (addrbook-get-prop-index n-fields addrbook-field-for-sort))
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

;;;; * Fast selection

(defun addrbook-fast-selection (names prompt)
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
      (set-buffer (get-buffer-create " *AddrBook Groups*"))
      (delete-other-windows)
      (split-window-vertically)
      (switch-to-buffer-other-window (get-buffer-create " *AddrBook Groups*"))
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

(defun addrbook-select-type (attr-name)
  (let* ((property (addrbook-get-property attr-name))
         (prop-types (addrbook-get-prop-parameter property "type")))
    (let (type-names type letter result)
      (dolist (type prop-types)
        (setq type-names
              (cons (cdr type) type-names)))
      (setq type-names (reverse type-names))
      (setq letter (addrbook-fast-selection type-names "Select attribute type"))
      (if letter
          (dolist (type type-names)
            (if (equal letter
                       (cadr type))
                (setq result (car type)))))
      result)))

(defun addrbook-select-non-existing-type (attr)
  (let* ((attr-name (vcard-attr-get-name attr))
         (property (addrbook-get-property attr-name))
         (prop-types (addrbook-get-prop-parameter property "type"))
         (attr-types (vcard-attr-get-parameter attr "type")))
    (let (type-names type letter result)
      (dolist (type prop-types)
        (if (not (member (car type) attr-types))
            (setq type-names
                  (cons (cdr type) type-names))))
      (setq type-names (reverse type-names))
      (setq letter (addrbook-fast-selection type-names "Select attribute type"))
      (if letter
          (dolist (type type-names)
            (if (equal letter
                       (cadr type))
                (setq result (car type)))))
      result)))

(defun addrbook-select-existing-type (attr)
  (let* ((attr-name (vcard-attr-get-name attr))
         (property (addrbook-get-property attr-name))
         (prop-types (addrbook-get-prop-parameter property "type"))
         (attr-types (vcard-attr-get-parameter attr "type")))
    (let (type-names type letter result)
      (dolist (type prop-types)
        (if (member (car type) attr-types)
            (setq type-names
                  (cons (cdr type) type-names))))
      (setq type-names (reverse type-names))
      (setq letter (addrbook-fast-selection type-names "Select attribute type"))
      (if letter
          (dolist (type type-names)
            (if (equal letter
                       (cadr type))
                (setq result (car type)))))
      result)))

(defun addrbook-select-group ()
  "Select a group interactively and return its symbol"
  (let (names group group-elt letter result)
    ;; Build the names list
    (dolist (group-elt addrbook-properties)
      (setq names
            (cons (list (addrbook-get-group-name group-elt)
                        (addrbook-get-group-letter group-elt))
                  names)))
    (setq names (reverse names))
    ;; Call the fast menu function to get the desired group
    (setq letter (addrbook-fast-selection names "Select group"))
    (dolist (group-elt addrbook-properties)
      (if (and (addrbook-get-group-letter group-elt)
               (equal letter (addrbook-get-group-letter group-elt)))
          (setq result (addrbook-get-group-symbol group-elt))))
    result))

(defun addrbook-select-property (group-symbol)
  "Select a property interactively from GROUP and return its name"
  (let* ((group (addrbook-get-group group-symbol))
         (group-props (addrbook-get-group-props group))
         names attr attr-elt letter result)
    ;; Build the names list
    (dolist (prop group-props)
      (if (and (not (member (addrbook-get-prop-name prop) addrbook-required-attrs))
               (addrbook-get-prop-letter prop))
          (setq names
                (cons (list (addrbook-get-prop-title prop)
                            (addrbook-get-prop-letter prop))
                      names))))
    (setq names (reverse names))
    ;; Call the fast menu function to get the desired group
    (setq letter (addrbook-fast-selection names "Select property"))
    (dolist (prop group-props)
      (if (and (addrbook-get-prop-letter prop)
               (equal letter (addrbook-get-prop-letter prop)))
          (setq result (addrbook-get-prop-name prop))))
    result))

(defun addrbook-select-field (group-symbol prop-name)
  "Select a field interactively from PROP-NAME"
  (let* ((group (addrbook-get-group group-symbol))
         (group-props (addrbook-get-group-props group))
         (property (assoc prop-name group-props))
         (prop-fields (addrbook-get-prop-fields-list property))
         letter field result i)
    (setq letter (addrbook-fast-selection prop-fields "Select property field"))
    (dotimes (i (length prop-fields))
      (setq field (nth i prop-fields))
      (if (equal letter (addrbook-get-prop-field-letter field))
          (setq result i)))
    result))

;;;; ** Search functions

(defun addrbook-attr-matches-p (attr regexp)
  (let (result value
        (attr-values (vcard-attr-get-values attr)))
    (if (listp attr-values)
        (dolist (value attr-values)
          (if (string-match regexp value)
              (setq result t)))
      (setq result (string-match regexp attr-values)))
    result))

(defun addrbook-search-cards (regexp &optional properties)
  "Search for REGEXP in card data and return a list with the indexes
of matching cards.

PROPERTIES is a list of property names.
If PROPERTIES is specified and non-nil, the search is performed only in those
attributes."
  (let (card prop attr card-index attr-index result)
    (dotimes (card-index (length addrbook-cards))
      (setq card (addrbook-get-card card-index))
      (dotimes (attr-index (vcard-get-num-attributes card))
        (setq attr (vcard-get-attribute card attr-index))
        (if (and (or (not properties)
                     (member (vcard-attr-get-name attr) properties))
                 (addrbook-attr-matches-p attr regexp))
            (add-to-list 'result card-index))))
    (reverse result)))

;;;; * Entry points to the addressbook

;;;###autoload
(defun addressbook ()
  "Open the addressbook"
  (interactive)
  (if (and addrbook-force-addressbook-creation
           (not (file-exists-p addrbook-file)))
      (with-temp-file addrbook-file))
  (catch 'exit
    (let ((buffer (get-buffer addrbook-contact-buffer-name)))
      (if (not buffer)
        (let ((show-card-index 0)
              (user-input (if addrbook-ask-for-search
                              (read-from-minibuffer "Search for contact [RET goes to the summary]: ")
                            "")))
          (unless (addrbook-open)
            (throw 'exit t))
          (addrbook-sort-cards)
          (if (not (equal user-input ""))
              (let ((found-cards (addrbook-search-cards user-input)))
                (if found-cards
                    (setq show-card-index (car found-cards))
                  (message "No contacts found")
                  (throw 'exit t))
                ;; Goto the first card with matched data
                (addrbook-create-contact-buffer)
                (addrbook-contact-display-card show-card-index)
                (setq addrbook-modified-cards nil)
                (switch-to-buffer-other-window (get-buffer addrbook-contact-buffer-name))
                (setq buffer-read-only t)
                (setq addrbook-buffer buffer))
            ;; Goto the summary
            (addrbook-summary))))
      (addrbook-show-summary))))


;;;###autoload
(defun addressbook-create ()
  "Create a new contact into the addressbook and save it"
  (interactive)
  (addrbook-be-read-cards)
  (let ((new-card-index (addrbook-create-card-2)))
    (if new-card-index
        (addrbook-save-cards nil))))


(provide 'addressbook)

;;; addrbook.el ends here
