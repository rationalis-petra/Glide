;;;; commands.lisp

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

;; This file defines many commands that are commonly used by Glide. Examples may include:
;; • Loading/Saving a file
;; • Splitting a frame/view horizontally or vertically
;; • Closing a frame 
(defmacro defcommand (name (&body alist))
  (let ((title (cadr (assoc :title alist)))
        (documentation (cadr (assoc :documentation alist))))
  `(progn
     (setf (gethash ,name *commands*)
           (make-instance 'command
                          :title title
                          :documentation documentation)))))

(defvar *commands* (make-hash-table))

(defclass command ()
  ((function
    :initarg :function)
   (args
    :initarg :args)
   (title
    :initarg :title)
   (documentation
    :initarg :documentation))
  (:documentation "A command represents a common lisp function which is exposed
  to the user via, e.g. a keybind, menu-option or button"))

(defun open-file (window)
  (gtk4:file-dialog-open
   (gtk4:make-file-dialog)
   (gtk-widget window)
   nil
   (lambda (file) (message-info (format nil "file was: ~A" file)))
   nil))

;; (defcommand (base open-file)
;;   (:function #'open-file)
;;   (:title "Open File"))

;; (defcommand (base save-file)
;;   (:function () save-file)
;;   (:title "Save File")
;;   (:when (lambda (view) (can-save view))))
