(in-package :glide)

(defclass view ()
  ((model
    :initarg :model
    :accessor view-model
    :documentation "The data that this view looks at")
   (widget
    :accessor view-widget
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
(defgeneric model-updated (view)
  (:documentation "Invoked on a view when the underlying model is updated")
  (:method (view)
    (error (format nil "View ~A should implement model-updated." view))))


(defgeneric view-commands (view)
  (:documentation "Return a set of command palette commands which the view
  provides. These will only be available when the specific view instance is
  in focus.")
  (:method (view) nil))
