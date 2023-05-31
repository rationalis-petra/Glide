(in-package :glide)


(defclass text-model (model)
  ((text-buffer
    :accessor text-model-buffer))
  (:documentation "Wraps a GTK4 text buffer"))


