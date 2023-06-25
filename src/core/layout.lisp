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
  ((gtk-widget
    :accessor gtk-widget)
   (location-hint
    :accessor location-hint
    :initform :centre)
   (inner-widget
    :accessor inner-widget)
   (parent
    :accessor parent
    :initarg :parent)))

(defclass single-layout (layout)
  ((child
    :reader child)))

(defclass paned-layout (layout)
  ((start-child
    :reader start-child
    :initarg :start-child)
   (end-child
    :reader end-child
    :initarg :end-child)
   (orientation
    :reader orientation
    :initarg :orientation)))

(defclass tabbed-layout (layout)
  ((children
    :accessor children
    :initarg :children)))

;; Frame - 

(defclass frame ()
  ((view
    :initarg :view
    :accessor view
    :type view)
   (location-hint
    :accessor location-hint
    :initform :centre)
   (modeline
    :reader modeline)
   (gtk-widget
    :reader gtk-widget)
   (parent
    :accessor parent)
   (focus-controller
    :initarg focus-controller)))

;; Modeline - visible at the bottom of a frame

(defclass modeline ()
  ((gtk-widget
    :accessor gtk-widget)))


(defgeneric (setf child) (child parent)
  (:method (child (parent single-layout))
    (when (slot-boundp parent 'child)
      (gtk4:box-remove
       (inner-widget parent)
       (gtk-widget (child parent))))
    (gtk4:box-append (inner-widget parent) (gtk-widget child))
    (setf (slot-value parent 'child) child)
    (setf (parent child) parent)))

(defgeneric (setf start-child) (child layout)
  (:method (child (layout paned-layout))
    (setf (gtk4:paned-start-child (inner-widget layout)) (gtk-widget child))
    (setf (slot-value layout 'start-child) child)
    (setf (parent child) layout)))

(defgeneric (setf end-child) (child layout)
  (:method (child (layout paned-layout))
    (setf (gtk4:paned-end-child (inner-widget layout)) (gtk-widget child))
    (setf (slot-value layout 'end-child) child)
    (setf (parent child) layout)))

(defmethod (setf inner-widget) (widget (layout layout))
  (when (slot-boundp layout 'inner-widget)
    (gtk4:box-remove (gtk-widget layout) (inner-widget layout)))
  (setf (slot-value layout 'inner-widget) widget)
  (gtk4:box-append (gtk-widget layout) (inner-widget layout)))

(defmethod initialize-instance :after
    ((layout single-layout) &key child)
  (let* ((gtk-widget (gtk4:make-box :spacing 0 :orientation gtk4:+orientation-vertical+))
         (inner-widget (gtk4:make-box :spacing 0 :orientation gtk4:+orientation-vertical+))
         (wrapped-child (wrap-child-for layout child)))

    (setf (gtk-widget layout) gtk-widget)
    (setf (inner-widget layout) inner-widget)
    (setf (child layout) wrapped-child)

    layout))

(defmethod initialize-instance :after
    ((layout paned-layout) &key orientation start-child end-child)
  (let* ((gtk-widget
           (gtk4:make-box :spacing 0 :orientation gtk4:+orientation-vertical+))
         (paned (gtk4:make-paned
                 :orientation (if (eq orientation :vertical)
                                  gtk4:+orientation-vertical+
                                  gtk4:+orientation-horizontal+)))
         (wrapped-start-child (wrap-child-for layout start-child))
         (wrapped-end-child (wrap-child-for layout end-child)))

    (setf (slot-value layout 'orientation) orientation)
    (setf (gtk-widget layout) gtk-widget)
    (setf (inner-widget layout) paned)
    (setf (start-child layout) wrapped-start-child)
    (setf (end-child layout) wrapped-end-child)

    layout))

(defmethod initialize-instance :after
    ((layout tabbed-layout) &key children)
  (let* ((gtk-widget (gtk4:make-box :spacing 0 :orientation gtk4:+orientation-vertical+))
         (notebook (gtk:make-notebook))
         (wrapped-children (mapcar #'wrap-child-for children)))

    (gtk4:box-append gtk-widget notebook)

    (setf (gtk-widget layout) gtk-widget)
    (setf (inner-widget layout) notebook)
    (iter (for child in wrapped-children)
      (gtk4:notebook-append-page
       notebook
       (gtk-widget child)
       (name child)))

    (setf (children layout) wrapped-children)
    layout))

(defgeneric swap-layout-to (layout class &key &allow-other-keys)
  (:documentation "Sets Internal class to provided class-designator"))
  
(defmethod swap-layout-to :before ((layout layout) class &key &allow-other-keys)
  (gtk4:box-remove (gtk-widget layout) (inner-widget layout)))

(defmethod swap-layout-to :before ((layout single-layout) class &key &allow-other-keys)
  (gtk4:box-remove (inner-widget layout) (gtk-widget (child layout))))

(defmethod swap-layout-to :before ((layout paned-layout) class &key &allow-other-keys)
  (setf (gtk4:paned-start-child (inner-widget layout)) nil)
  (setf (gtk4:paned-start-child (inner-widget layout)) nil))

(defmethod swap-layout-to :before ((layout tabbed-layout) class &key &allow-other-keys)
  (iter (for child in (children layout))
    (gtk4:notebook-remove-page
     (inner-widget layout)
     (gtk4:notebook-get-page (inner-widget layout)))))


(defmethod swap-layout-to ((layout layout) (class (eql 'single-layout))
                           &key child)
  (change-class layout 'single-layout)
  (let ((inner-widget (gtk4:make-paned :spacing 0 :orientation gtk4:+orientation-vertical+))
        (wrapped-child (wrap-child-for layout child)))
    (setf (inner-widget layout) inner-widget)
    (setf (child layout) wrapped-child)))

(defmethod swap-layout-to ((layout layout) (class (eql 'paned-layout))
                           &key orientation start-child end-child)
  (change-class layout 'paned-layout)
  (let ((inner-widget
          (gtk4:make-paned :orientation
                           (if (eq orientation :vertical)
                               gtk4:+orientation-vertical+
                               gtk4:+orientation-horizontal+)))
        (wrapped-start-child (wrap-child-for layout start-child))
        (wrapped-end-child (wrap-child-for layout end-child)))

    (setf (slot-value layout 'orientation) orientation)
    (setf (inner-widget layout) inner-widget)
    (setf (start-child layout) wrapped-start-child)
    (setf (end-child layout) wrapped-end-child)))

(defmethod swap-layout-to ((layout layout) (class (eql 'tabbed-layout))
                           &key children)
  (message-error "Swap child to tabbed layout not implemented"))

(defgeneric wrap-child-for (layout child))
(defmethod wrap-child-for (layout (child view))
  (let ((frame (make-instance 'frame :view child))) frame))
(defmethod wrap-child-for (layout (child frame)) child)
(defmethod wrap-child-for (layout (child layout)) child)

(defgeneric location-hints (layout)
  (:documentation
   "If this layout/frame was created with a layout preference, returns that
  preference. If this is a 'container' layout, will additionally return the
  preferences of its' children."))

(defmethod location-hints ((frame frame))
  (list (location-hint frame)))
(defmethod location-hints ((layout single-layout))
  (list (location-hint layout)))
(defmethod location-hints ((layout paned-layout))
  (append (location-hint layout) (location-hints (start-child layout) (end-child layout))))
(defmethod location-hints ((layout paned-layout))
  (apply #'append (cons (location-hint layout) (mapcar #'location-hints (children layout)))))


(defmethod transient-p ((layout layout)) nil)
(defmethod transient-p ((frame frame)) (transient-p (view frame)))
;; hierarchy of layout hints:
;; + tab
;; + left
;; + right
;; + top
;; + bottom
;; + centre

;; (declaim (ftype (function
;;                  (layout view &key
;;                          (layout-preference keyword))
;;                  layout)
;;                 layout-add-child))
;; (defun layout-add-child (layout child &key (layout-preference :none))
;;   "Will add CHILD to LAYOUT. The location and manner in which the CHILD is added
;;                  is dependent both on where the focus is and the value of
;;                  LAYOUT-PREFERENCE. The LAYOUT will traverse downwards into the
;;                  child that has focus until it finds either a single-layout or a
;;                  frame. When this is the case, it will insert the child 'next
;;                  to' the focus frame in a manner dependent on
;;                  LAYOUT-PREFERENCE.

;; LAYOUT-PREFERENCE can be one of :BELOW :ABOVE :LEFT :RIGHT or :TAB"
;;   (let ((frame (make-instance 'frame :view child))
;;         (focus-child (focus-child layout)))
;;     (with-slots (inner-widget layout-type children) layout
;;       (setf (close-fn frame) (lambda () (layout-remove-child layout frame)))

;;       (typecase focus-child
;;         (layout (layout-add-child focus-child child :layout-preference layout-preference))
;;         (frame ()))
;;       ;; TODO: branching logic based on layout-type!
;;       ;; (when is single) 
;;       layout)))


(declaim (ftype (function
                 (layout view &key
                         (layout-location keyword))
                 layout)
                layout-add-child-absolute))
;; TODO LAYOUT-REPLACE as an option.
(defgeneric layout-add-child-absolute (layout child &key layout-location) 
  (:documentation
   "Unlike LAYOUT-ADD-CHILD, which will add a child in relative to the current
                 (focused) view, LAYOUT-ADD-CHILD-ABSOLUTE will add a new child
                 with LAYOUT-LOCATION relative to the root layout (assumed to
                 be the one called in the function). These are shown below in a
                 diagram: 

                             +---+------OUTSIDE-------+---+
                             |   |        TOP         |   |
                             | L +--------------------+ R |
                             | E |                    | I |
                             | F |       CENTRE       | G |
                             | T |                    | H |
                             |   |                    | T |
                             |   +--------------------+   |
                             |   |       BOTTOM       |   |
                             +----------------------------+

                 If a space in the layout doesn't exist, then the layout will
                 attempt to create one. If there is already a widget in the
                 location where you are attempting to put the child, then tabs
                 will be added and both widgets will be able to occupy that
                 position in the layout."))


(defmethod layout-add-child-absolute ((layout single-layout) child &key (layout-location :none)) 
  (with-slots (gtk-widget inner-widget (old-child child)) layout
    (match layout-location
      ((or :bottom :top :left :right)
       (let ((paned (gtk4:make-paned
                    :orientation (if (or (eq layout-location :left) (eq layout-location :right))
                                     gtk4:+orientation-horizontal+
                                     gtk4:+orientation-vertical+)))
             (wrapped-old-child (wrap-child-for layout old-child))
             (wrapped-new-child (wrap-child-for layout child)))
             

         (setf (gtk4:paned-start-child paned)
               (gtk-widget
                (if (or (eq layout-location :top) (eq layout-location :left))
                    wrapped-new-child wrapped-old-child)))
         (setf (gtk4:paned-end-child paned)
               (gtk-widget
                (if (or (eq layout-location :top) (eq layout-location :right))
                    wrapped-old-child wrapped-new-child)))

         (gtk4:box-remove gtk-widget inner-widget)
         (gtk4:box-remove inner-widget (gtk-widget wrapped-old-child))
         (gtk4:box-append gtk-widget paned)

         (setf inner-widget paned)

         (change-class layout 'paned-layout)
         (setf (start-child layout)
               (if (or (eq layout-location :top) (eq layout-location :right))
                    wrapped-new-child wrapped-old-child))
         (setf (end-child layout)
               (if (or (eq layout-location :top) (eq layout-location :right))
                   wrapped-old-child wrapped-new-child))
         (setf (orientation layout) :horizontal)))
      
      (:none
       (if (transient-p old-child)
           (let ((wrapped-new-child (wrap-child-for layout child)))
             (setf (child layout) wrapped-new-child))
           (swap-layout-to layout 'paned-layout
                           :start-child old-child
                           :end-child child
                           :orientation :horizontal)
           ;; (let ((paned (gtk4:make-paned
           ;;               :orientation gtk4:+orientation-horizontal+))
           ;;       (wrapped-old-child (wrap-child-for layout old-child))
           ;;       (wrapped-new-child (wrap-child-for layout child)))

           ;;   (gtk4:box-remove inner-widget (gtk-widget wrapped-old-child))
           ;;   (setf (gtk4:paned-start-child paned) (gtk-widget wrapped-old-child))
           ;;   (setf (gtk4:paned-end-child paned) (gtk-widget wrapped-new-child))

           ;;   (gtk4:box-remove gtk-widget inner-widget)
           ;;   (gtk4:box-append gtk-widget paned)

           ;;   (setf inner-widget paned)

           ;;   (change-class layout 'paned-layout)
           ;;   (setf (start-child layout) wrapped-old-child)
           ;;   (setf (end-child layout) wrapped-new-child)
           ;;   (setf (orientation layout) :horizontal))
           ))

      ((or :outside :centre)
       ;; convert to tabbed layout
       (error "layout-add-child-absolute ((single layout) :outside) not yet implemented")))
    layout))

(defmethod layout-add-child-absolute ((layout paned-layout) child &key (layout-location :none)) 
  (with-slots (gtk-widget inner-widget orientation start-child end-child) layout
    (match (cons orientation layout-location)
      ((cons :horizontal (or :bottom :top))
       (cond
         ((member :bottom (location-hints (start-child layout)))
          (error "TODO"))
         ((member :bottom (location-hints (end-child layout)))
          (error "TODO"))
         ((and (member :left (location-hints (start-child layout)))
               (member :right (location-hints (end-child layout))))
          (error "TODO"))
          ;; create a new default view 
         (t ;; remove children from pane
            (setf (gtk4:paned-start-child inner-widget) nil)
            (setf (gtk4:paned-end-child inner-widget) nil)
            (setf (gtk4:orientable-orientation inner-widget) gtk4:+orientation-vertical+)

            ;; create new layout
            (let* ((pane (gtk4:make-paned :orientation
                                          gtk4:+orientation-vertical+))
                   (new-layout (make-instance 'paned-layout
                                              :orientation orientation
                                              :start-child start-child
                                              :end-child end-child))
                   (wrapped-new-layout (wrap-child-for layout new-layout))
                   (wrapped-child (wrap-child-for layout child)))

              (if (eq :bottom layout-location)
                  (progn
                    (setf start-child wrapped-new-layout)
                    (setf end-child wrapped-child)
                    (setf (gtk4:paned-start-child inner-widget) (gtk-widget wrapped-new-layout))
                    (setf (gtk4:paned-end-child inner-widget) (gtk-widget wrapped-child)))
                  (progn
                    (setf start-child wrapped-child)
                    (setf end-child wrapped-new-layout)
                    (setf (gtk4:paned-start-child inner-widget) (gtk-widget wrapped-child))
                    (setf (gtk4:paned-end-child inner-widget) (gtk--widget wrapped-new-layout))))))))

      (t (error "TODO-match")))
    layout))

(defun remove-child (parent child)
  (message-info "removing child"))


(defmethod initialize-instance :after ((frame frame) &key view)
  (let ((box (gtk4:make-box
              :orientation gtk4:+orientation-vertical+
              :spacing 10))
        (focus-controller (gtk4:make-event-controller-focus)))

    (gtk4:widget-add-css-class box "frame")
    (gtk4:widget-add-controller box focus-controller)
    ;; (setf (gtk4:widget-valign (gtk-widget view)) gtk4:+align-start+)
    ;; (setf (gtk4:widget-valign box) gtk4:+align-bottom+)

    (setf (slot-value frame 'gtk-widget) box)
    (setf (slot-value frame 'modeline)
          (make-instance 'modeline :frame frame))

    (gtk4:box-append box (gtk-widget view))
    (gtk4:box-append box (gtk-widget (modeline frame)))))


(defmethod initialize-instance :after ((modeline modeline) &key frame)
  (setf (gtk-widget modeline)
        (gtk4:make-box
         :orientation gtk4:+orientation-horizontal+
         :spacing 10))
  (gtk4:widget-add-css-class (gtk-widget modeline) "modeline")
  (setf (gtk4:widget-valign (gtk-widget modeline)) gtk4:+align-end+)
  (setf (gtk4:widget-vexpand-p (gtk-widget modeline)) t)

  (let ((label (gtk4:make-label :str (name (view frame)))))
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
                    (funcall (remove-child (parent frame) frame))))
    (gtk4:box-append (gtk-widget modeline) close-btn)))

