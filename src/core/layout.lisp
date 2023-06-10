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
    :accessor children)))


(defclass frame ()
  ((current-view
    :initarg :view
    :accessor frame-view)
   (modeline
    :reader frame-modeline)
   (close-fn
    :accessor close-fn)
   (gtk-widget
    :reader gtk-widget)))


(declaim (ftype (function (frame) layout) make-single-layout))
(defun make-single-layout (child)
  (let* ((box (gtk:make-box :spacing 0
                           :orientation gtk4:+orientation-vertical+))
         (layout (make-instance 'layout
                                :gtk-widget box
                                :layout-type :single
                                :children (list child))))
    (gtk4:box-append box (gtk-widget child))
    (setf (close-fn child)
          (lambda ()
            (remove-if (lambda (elem) (eq child elem)) (children layout) )
            (gtk4:box-remove box (gtk-widget child))))

    layout))


;(declaim (ftype (function (layout frame &key (orientation keyword)) layout) layout-add-child))
(defun layout-add-child (layout child &key orientation)
  (with-slots (gtk-widget layout-type children) layout
    (setf (close-fn child) 
          (lambda ()
            (remove-if (lambda (elem) (eq child elem)) children)
            (gtk4:box-remove gtk-widget (gtk-widget child))))

  ;; TODO: branching logic based on layout-type!
    (gtk:box-append gtk-widget (gtk-widget child))
    (push child children)
    (setf layout-type orientation)))


(defmethod initialize-instance :after ((frame frame) &key view)
  (let ((box (gtk4:make-box
              :orientation gtk4:+orientation-vertical+
              :spacing 10))
        (modeline (gtk4:make-box
              :orientation gtk4:+orientation-horizontal+
              :spacing 10))
        (label (gtk4:make-label :str (view-name view)))
        (fill (gtk4:make-box :spacing 0 :orientation gtk4:+orientation-horizontal+))
        (close-btn (gtk4:make-button :label "X")))
    (setf (slot-value frame 'modeline) modeline)
    (setf (slot-value frame 'gtk-widget) box)

    (gtk4:box-append modeline label)
    (iter (for widget in (modeline-widgets view))
      (gtk4:box-append modeline widget))

    (setf (gtk4:widget-hexpand-p fill) t)
    (gtk4:box-append modeline fill)
    (gtk4:box-append modeline close-btn)
    (setf (gtk4:widget-hexpand-p box) t)
    (setf (gtk4:widget-vexpand-p box) t)

    (gtk4:box-append box (gtk-widget view))
    (gtk4:box-append box modeline)

    ;; Do this last, so that 'mk-close' gets an (almost) fully initialized frame
    (gtk4:connect close-btn "clicked"
                  (lambda (button)
                    (declare (ignore button))
                    (funcall (close-fn frame))))))
