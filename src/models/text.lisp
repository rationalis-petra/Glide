(in-package :glide)


(defclass text-model (model)
  ((gtk-buffer
    :accessor gtk-buffer))
  (:documentation "Wraps a GTK4 text buffer"))


(defmethod initialize-instance ((model text-model) &key)
  (setf (gtk-buffer model) (gtk4:make-text-buffer :table nil)))


(defun insert-at-cursor (model string)
  (gir:invoke
   ((gtk-buffer model) :insert-at-cursor)
   (string string)
   (utf-8-byte-length string)))
