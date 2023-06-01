(in-package :glide)


(defclass dashboard-view (view)
  ()
  (:documentation "A dashboard or start-screen"))


(defmethod initialize-instance :after ((view dashboard-view) &key model)
  (declare (ignore model))
  (flet ((relpath (name)
           ;; TODO: change this to home/config dir
           (asdf:system-relative-pathname (asdf:find-system :glide) name)))

    (let ((image (gtk4:make-image :filename (namestring (relpath "assets/glider.png")))))
      (setf (gtk4:widget-size-request image) '(256 256))
      ; (setf (gtk4:picture-can-shrink-p image) t)
      ; (setf (gtk4:picture-keep-aspect-ratio-p image) t)
      (setf (gtk-widget view) image))))
