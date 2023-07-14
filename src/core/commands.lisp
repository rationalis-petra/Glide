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

(defclass command ()
  ((function
    :initarg :function)
   (args
    :initarg :args)
   (title
    :accessor title
    :initarg :title)
   (documentation
    :initarg :documentation))
  (:documentation "A command represents a common lisp function which is exposed
  to the user via, e.g. a keybind, menu-option or button"))

(defclass command-group ()
  ((elements
    :accessor elements
    :initarg :elements
    :initform nil
    :documentation "Elements contained within this command-group: either
  commands, or command sub-groups.") 
   (enabled
    :initarg :enabled
    :initform t)
   (documentation
    :initarg :documentation))
  (:documentation "A command group reprsents a set of commands. Command groups
  can be enabled or disabled depending on the current view & mode."))


(defparameter *commands*
  (make-instance 'command-group :enabled t))

(defmacro plugin-commands (&body args)
  ;; TODO: make this macro hygenic! (I think that's necessary?)
  `(macrolet
       ((command-group (name &rest args)
          "Produce a plist entry "
          (let ((when-form (cadr (assoc :when args)))
                (elements-forms (cdr (assoc :elements args))))
            `(list ,name (make-instance
                          'command-group
                          :enabled ,when-form
                          :elements (append ,@elements-forms)))))
        (command (name &rest args)
          "Produce a plist entry for a command group"
          (let ((function (cadr (assoc :function args)))
                (title (cadr (assoc :title args))))
            `(list ,name (make-instance
                          'command
                          :function ,function
                          :title ,title)))))
     (append ,@args)))

(defmacro command (name &rest args)
  (declare (ignore name args))
  (error "can only use command macro inside body of plugin-commands macro"))

(defmacro command-group (name &rest args)
  (declare (ignore name args))
  (error "can only use command-group macro inside body of plugin-commands macro"))
