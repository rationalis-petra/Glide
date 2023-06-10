(in-package :glide)

(defclass view ()
  ((model
    :accessor view-model
    :initarg :model
    :documentation "The data that this view looks at")
   (modeline-widgets
    :reader modeline-widgets
    :initform nil
    :documentation "Widgets to put on the modeline")
   (gtk-widget
    :accessor gtk-widget
    :documentation "The gtk4 widget that this view")))


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
(defgeneric view-name (view)
  (:documentation "Invoked on a view when the underlying model is updated")
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
