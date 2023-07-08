;;;; view.lisp

;; Copyright (C) 2023 Connor Redfern
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :glide)

;; Keymaps
;; Glide aims to suport
;; • Modal Editing
;; • Non Modal Editing
;; • Non text views
;; • Mouseclicks etc.
;; • Chords
;; 
;; Implementation
;; keymap → mapping of keys to values, may have multiple states
;; 
;; keystate → tracks what keys have been pressed (at what times) & current state. 
;;   also tracks current state (normal, insert, visual, etc.)
;; 


;(defclass keymap () ())

(defclass keymap ()
  ((mode-maps)) ;; alist of keymaps, by state
  (:documentation "A keymap with different modes, e.g. insert, normal, etc."))

(defclass keystate ()
  ((keymap)
   (current-key-seq)
   (current-state)))

(defclass keyseq ()
  ()
  (:documentation ""))

(defun parse-key-seq (string))


(defgeneric key-action (keymap keystate key))
(defmethod key-action ((keymap keymap) keystate key)
  (let ((state-map (assoc keystate (mode-maps keymap))))
    ( )))

(defgeneric add-key (keymap state keystring))

;; set leader
(defvar *leader-keys* '(:normal . "<Space>"))
(defvar *local-leader-keys* '(:normal . ";"))


;; (define-keys
;;   :keymaps (glyph:glyph-map)
;;   :states (:normal :insert)
;;   "ee" #'eval-glyph)

(defmacro define-keys (&key keymaps (states :insert) keybinds)
  (unless keymap (error "Did not provide keymap to macro define-keys"))

  (let ((mp (gensym)) (state (gensym)) (bind (gensym)))
    `(iter (for (,mp in ,keymaps))
           (iter (for ,state in ,states)
                 (iter (for ,bind in ,keybinds)
                       (add-key ,mp ,state ,bind))))))

(defmacro defkeymap (name &key (states :insert))
  `(defvar ,name (make-instance 'keymap)))
