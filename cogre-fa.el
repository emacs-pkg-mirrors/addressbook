;;; cogre-fa.el --- Finite Automata support for COGRE

;;; Copyright (C) 2009 Jose E. Marchesi

;; Author: Jose E. Marchesi <jemarch@gnu.org>
;; Keywords: automata

;; This file is not part of GNU Emacs.

;; This is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This software is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.
 
;;; Commentary:
;;
;; Provides Finite Automata edition for COGRE.

(require 'cogre)
(require 'cogre-mode)

;;; Code:

(defclass cogre-fa-graph (cogre-graph)
  ()
  "A graph depicting a Finite Automata.")

(defmethod cogre-fa-graph-transition-table ((graph cogre-fa-graph))
  (with-temp-buffer
    (let ((e (oref (this) elements)))
     
      (buffer-substring-no-properties (point-min) (point-max)))))

(defclass cogre-fa-state (cogre-node)
  ((name-default :initform "q")
   (blank-lines-top :initform 0)
   (blank-lines-bottom :initform 0)
   (final-p :initform nil :custom boolean
            :documentation "Boolean indicating if this state is final.")
   (initial-p :initform nil :custom boolean
              :documentation "Boolean indicating if this is the initial state.")
   (alignment :initform center))
  "A Finite Automata state.")

(defmethod cogre-node-slots ((state cogre-fa-state))
  (let ((result nil))
    (if (oref state final-p)
        (list (list (concat "-" (propertize "f" 'face 'cogre-fa-final-face))))
      (if (oref state initial-p)
          (list (list (concat "-" (propertize "i" 'face 'cogre-fa-initial-face))))
        (list (list "--"))))))

(defface cogre-fa-link-label-face '((((class color) (background dark))
                                     (:foreground "blue"))
                                    (((class color) (background light))
                                     (:foreground "blue")))
  "Face used for the link labels."
  :group 'cogre)

(defface cogre-fa-final-face '((((class color) (background dark))
                                (:foreground "red"))
                               (((class color) (background light))
                                (:foreground "red")))
  "Face used for the final attribute display."
  :group 'cogre)

(defface cogre-fa-initial-face '((((class color) (background dark))
                                  (:foreground "yellow"))
                                 (((class color) (background light))
                                  (:foreground "yellow")))
  "Face used for the initial attribute display."
  :group 'cogre)

(define-key cogre-mode-map "l" 'cogre-set-link-label)

(defun cogre-set-link-label (link label)
  "Set the label of the current LINK to LABEL."
  (interactive (let ((l (cogre-link-at-point-interactive)))
                 (list l (read-string "New Label: " ""
                                      nil (oref l label)))))
  (cogre-erase link)
  (if (equal label "")
      (oset link label nil)
    (oset link label label))
  (if (interactive-p)
      (cogre-render-buffer cogre-graph)))

(defclass cogre-labeled-link (cogre-link)
     ((label :initarg :label
             :initform nil
             :type (or null string)
             :custom string
             :documentation "The label of the link.
If non-null, a string containing the label for this link."))
     "A labeled link.")

(defmethod cogre-render ((link cogre-labeled-link))
  "Render LINK in the current graph."
  (call-next-method)
  (with-slots (start end label) link
    (let* ((hd (cogre-node-horizontal-distance start end))
           (vd (cogre-node-vertical-distance start end))
           linkcoords dir)
      ;; Calculate starting points in relation to our attached nodes.
      (if (> (* hd (oref link horizontal-preference-ratio)) vd)
	  ;; In this case, the X delta is larger than the Y delta,
	  ;; so the line is going mostly left/right.
	  (setq linkcoords (cogre-choose-horizontal-link-anchors start end)
		dir 'horizontal)
	(setq linkcoords (cogre-choose-vertical-link-anchors start end)
	      dir 'vertical))
      (let* ((x1 (nth 0 linkcoords))
             (y1 (nth 1 linkcoords))
             (x2 (nth 2 linkcoords))
             (y2 (nth 3 linkcoords))
             (lx (+ x1 (/ (- x2 x1) 2) (% (- x2 x1) 2)))
             (ly (+ y1 (/ (- y2 y1) 2) (% (- y2 y1) 2))))
        (when label
          (if cogre-erase-mode
              (cogre-erase-rectangle lx ly (length label) 1)
            (picture-goto-coordinate lx ly)
            (picture-insert-rectangle
             (list (propertize label 'face 'cogre-fa-link-label-face))
             nil)))))))
            
(defclass cogre-fa-transition (cogre-labeled-link)
  ((symbol :initform "0" :custom string)
   (end-glyph :initform [("/\\ ")
                         ("\\/ ")
                         ("<") (">")])
   (horizontal-preference-ratio :initform 1))
  "This type of link indicates a state transition.")

;;; End of cogre-fa.el
