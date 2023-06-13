(in-package :glide)

(defclass abstract-list-view (view)
  ()
  (:documentation ""))

(defgeneric make-list-item-widget (view))

(defgeneric update-list-item-widget (view item widget))

(defmethod initialize-instance :after ((view abstract-list-view) &key model) ;; selection-type?
  ;(declare (type model list-model))
  (let* ((factory (gtk4:make-signal-list-item-factory))
         (list-view (gtk4:make-list-view :model
                                         (gtk4:make-single-selection :model (gtk-list model))
                                         :factory factory)))

    (setf (gtk-widget view) list-view)

    (gtk4:connect factory "setup"
                  (lambda (factory item)
                    (declare (ignore factory))
                    (setf (gtk4:list-item-child item) (make-list-item-widget view))))
    (gtk4:connect factory "bind"
                  (lambda (factory item)
                    (declare (ignore factory))
                    (let ((widget (gtk4:list-item-child item))
                          (lisp-item (gethash (gtk4:list-item-item item) (lisp-vals model))))
                      (update-list-item-widget view lisp-item widget))))
    (gtk4:connect factory "unbind"
                  (lambda (factory item)
                    (declare (ignore factory item)))) 
    (gtk4:connect factory "teardown"
                  (lambda (factory item)
                    (declare (ignore factory item))))))

;(defclass list-view (abstract-list-view))
