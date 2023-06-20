;;;; layout.lisp

;; Copyright (C) 2023 Connor Redfern
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :glide)

;; This file contains 3 classes, which help in the management and layout of views 
;; Layout
;; Frame
;; Modeline

(defclass layout ()
  ((parent
    :initarg :parent
    :reader :parent)
   (gtk-widget
    :initarg :gtk-widget
    :reader gtk-widget)
   (layout-type
    :initarg :layout-type
    :reader layout-type
    :type (or (eql :single) (eql :vertical) (eql :horizontal) (eql :tab)))
   (children
    :initarg :children 
    :accessor children)))

(declaim (ftype (function (t view) layout) make-single-layout))
(defun make-single-layout (parent child)
  (let* ((box (gtk:make-box :spacing 0
                            :orientation gtk4:+orientation-vertical+))
         (frame (make-instance 'frame :view child))
         (layout (make-instance 'layout
                                :gtk-widget box
                                :parent parent
                                :layout-type :single
                                :children (list frame))))

    (gtk4:widget-add-css-class (gtk-widget frame) "frame")
    (gtk4:box-append box (gtk-widget frame))
    (setf (close-fn frame)
          (lambda ()
            (remove-if (lambda (elem) (eq frame elem)) (children layout))
            (gtk4:box-remove box (gtk-widget frame))))

    layout))

(declaim (ftype (function (layout (or :vertical :horizontal :tab)) null) (setf layout-type)))
(defun (setf layout-type) (layout new-type)
  (unless (eql new-type (layout-type layout))
    (flet ((swap-widget-to (new-widget append-fn)
             ;; TODO: this wlil cause a bug!
             (gtk4:box-remove (gtk-widget (parent layout)))
             (iter (for child in (children layout))
               (append-fn child))))
      (ecase new-type
        (:vertical
         (swap-widget-to
          (gtk:make-box :spacing 0 :orientation gtk4:+orientation-vertical+)
          #'gtk4:box-append))
        (:horizontal
         (swap-widget-to
          (gtk:make-box :spacing 0 :orientation gtk4:+orientation-horizontal+)
          #'gtk4:box-append))
        (:tab
         (swap-widget-to
          (gtk:make-notebook)
          (lambda (notebook child)
            (gtk4:notebook-append-page
             notebook
             child
             (write-to-string (type-of notebook))))))))))


;; TODO: change layout-add-child to accept a view!
;; (declaim (ftype (function (layout frame &key (orientation keyword)) layout) layout-add-child))
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


(defclass frame ()
  ((view
    :initarg :view
    :accessor view
    :type view)
   (modeline
    :reader modeline)
   (close-fn
    :accessor close-fn)
   (gtk-widget
    :reader gtk-widget)))


(defmethod initialize-instance :after ((frame frame) &key view)
  (let ((box (gtk4:make-box
              :orientation gtk4:+orientation-vertical+
              :spacing 10)))

    (setf (slot-value frame 'gtk-widget) box)
    (setf (slot-value frame 'modeline)
          (make-instance 'modeline :frame frame))

    (gtk4:box-append box (gtk-widget view))
    (gtk4:box-append box (gtk-widget (modeline frame)))))


(defclass modeline ()
  ((gtk-widget
    :accessor gtk-widget)))

(defmethod initialize-instance :after ((modeline modeline) &key frame)
  (setf (gtk-widget modeline)
        (gtk4:make-box
         :orientation gtk4:+orientation-horizontal+
         :spacing 10))
  (gtk4:widget-add-css-class (gtk-widget modeline) "modeline")

  (let ((label (gtk4:make-label :str (view-name (view frame)))))
    (gtk4:box-append (gtk-widget modeline) label))

  (let ((fill (gtk4:make-box :spacing 0
                             :orientation gtk4:+orientation-horizontal+)))
    (setf (gtk4:widget-hexpand-p fill) t)
    (gtk4:box-append (gtk-widget modeline) fill))

  ;; User-defined widgets 
  (iter (for widget in (modeline-widgets (view frame)))
        (gtk4:box-append (gtk-widget modeline) widget))

  (let ((close-btn (gtk4:make-button :label "×")))
    (gtk4:connect close-btn "clicked"
                  (lambda (button)
                    (declare (ignore button))
                    (funcall (close-fn frame))))
    (gtk4:box-append (gtk-widget modeline) close-btn)))
