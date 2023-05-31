(in-package :glide)

(defclass model ()
  ()
  (:documentation
   "A model subclass represents a data model"))


(defclass file-format ()
  ((extensions
    :accessor file-format-extensions)
   (loader
    :accessor file-format-loader)
   (saver
    :accessor file-format-saver)))

;; class generics
(defgeneric model-file-formats (model-class)
  (:method (model-class))
  (:documentation "File formats supported by a model"))

;; instance generics

;; (defgeneric model-file-formats (model-class) ())
