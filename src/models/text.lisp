(in-package :glide)


(defclass text-model (model)
  ((gtk-buffer
    :accessor gtk-buffer
    :initform (gtk4:make-text-buffer :table nil)))
  (:documentation "Wraps a GTK4 text buffer"))


;(defmethod initialize-instance ((model text-model) &key))

(declaim (ftype (function (text-model) string) text-model-string))
(defun text-model-string (model)
  "Get a string copy of the underlying text"
  (gtk4:text-buffer-text (gtk-buffer model)))

(declaim (ftype (function (text-model string) null) insert-at-cursor))
(defun insert-at-cursor (model string)
  (gir:invoke
   ((gtk-buffer model) :insert-at-cursor)
   string
   (utf-8-byte-length string))
  nil)

(declaim (ftype (function (text-model) t) text-model-end-iter))
(defun text-model-end-iter (model)
  (gtk4:text-buffer-end-iter (gtk-buffer model)))

(defun text-model-insert (model iterator string)
  (gir:invoke
   ((gtk-buffer model) :insert)
   iterator
   string
   (utf-8-byte-length string)))
