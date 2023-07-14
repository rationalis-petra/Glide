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

(defclass view ()
  ((model
    :accessor view-model
    :initarg :model
    :documentation "The data that this view looks at")
   (modes
    :accessor modes
    :initarg :modes
    :initform nil
    :documentation "The set of enabled modes")
   (modeline-widgets
    :reader modeline-widgets
    :initform nil
    :documentation "Widgets to put on the modeline")
   (keymaps
    :accessor keymaps
    :initform nil
    :documentation "The set of keymaps associated with this view")

   ;; 'Internal/limited Access' state: should only be accessed when 
   (keystate
    :accessor keystate
    :documentation "The current keystate - used for keeping track of key
    chords")
   (gtk-widget
    :accessor gtk-widget
    :documentation "The gtk4 widget that this view")
   (key-controller
    :accessor key-controller
    :documentation "The gtk4 key controller (used for keymaps)")
   (transient-p
    :accessor transient-p
    :initform nil
    :documentation "When true, this view will be replaced when the window
  attempts to add a new view at this location. When false, this view is
  persistent, and attempting to add a new view at the location of the current
  view will cause it to split."))
  (:documentation "A view defines a particular way of looking at data"))

(defclass mode ()
  ()
  (:documentation "A mode augments a view somehow: "))

;; class-level generics
(defgeneric view-supported-types (view-class)
  (:documentation "Returns a list of model classes which this view supports")
  (:method (view-class)
    (error (format nil "View Class ~A should implement supported types." view-class))))

(defgeneric view-menu-options (view-class)
  (:documentation "Any header menu options which should be present when
  instances of this class are present on a window.") 
  (:method (view-class) nil))

;; instance-level generics
(defgeneric name (view)
  (:documentation "Return the name of the object in question")
  (:method (view) (string (type-of view))))

(defgeneric model-updated (view)
  (:documentation "Invoked on a view when the underlying model is updated")
  (:method (view)
    (error (format nil "View ~A should implement model-updated." view))))

(defgeneric view-commands (view)
  (:documentation "Return a set of command palette commands which the view
  provides. These will only be available when the specific view instance is
  in focus.")
  (:method (view) nil))

(defgeneric add-keymap (view keymap)
  (:documentation "Associate a keymap with a particular view. Keymaps added
  later will have higher priority")
  (:method ((view view) keymap) (pushnew keymap (keymaps view))))

(defgeneric on-keypress (view keyval modifier-state)
  (:documentation "Called on a view whenever a key is pressed.")
  (:method ((view view) keyval modifier-state)
    ;; TODO: lookup values in keymap
    (let ((action (key-action keyval modifier-state)))
      ;; A command to run
      ;; A t, indicating that the state was updated
      ;; A nil, indicating no action was found
      (cond
        ((typep action 'command) (funcall action))
        ((null action) (call-next-method))
        (t nil)))))


(defmethod initialize-instance :after ((view view) &key model modes &allow-other-keys)
  (declare (ignore model modes))
  (let ((key-controller (gtk4:make-event-controller-key)))
    (gtk4:connect key-controller "key-pressed"
                  (lambda (controller keyval keycode state)
                    (declare (ignore controller state))
                    (on-keypress view keyval state)))
    (setf (key-controller view) key-controller)))


(defmethod (setf gtk-widget) (gtk-widget (view view))
  (setf (slot-value view 'gtk-widget) gtk-widget)
  (gtk4:widget-add-controller gtk-widget (key-controller view)))
