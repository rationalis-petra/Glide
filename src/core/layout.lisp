(in-package :glide)

;; stuff relating to window layout

(defclass layout ()
  ((gtk-widget
    :initarg :gtk-widget
    :reader gtk-widget)
   (layout-type
    :initarg :layout-type
    :reader layout-type)
   (children
    :initarg :children 
    :reader children)))


(defun make-single-layout (frame)
  (let ((box (gtk:make-box :spacing 0
                           :orientation gtk4:+orientation-vertical+)))
    (gtk4:box-append box (gtk-widget frame))
    (make-instance 'layout
                   :gtk-widget box
                   :layout-type :single
                   :children (list frame))))

(defun layout-add-child (layout child &key orientation)
  (with-slots (gtk-widget layout-type children) layout
  ;; TODO: branching logic based on layout-type!
    (gtk:box-append gtk-widget (gtk-widget child))
    (push child children)
    (setf layout-type orientation)))


(defclass frame ()
  ((current-view
    :initarg :view
    :accessor frame-view)
   (gtk-widget
    :reader gtk-widget)))


(defmethod initialize-instance :after ((frame frame) &key view)
  (let ((box (gtk4:make-box
              :orientation gtk4:+orientation-vertical+
              :spacing 10))
        (label (gtk4:make-label :str "modeline")))
    (gtk4:box-append box (gtk-widget view))
    (gtk4:box-append box label)
    
    (setf (slot-value frame 'gtk-widget) box)))
