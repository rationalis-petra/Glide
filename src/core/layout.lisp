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

(defgeneric active-view (parent))

(defmethod active-view ((parent frame)) (view parent))

(defmethod active-view ((parent single-layout))
  (active-view (child parent)))

(defmethod active-view ((parent paned-layout))
  (cond
    ((gtk4:widget-has-focus-p (gtk-widget (start-child parent))) (start-child parent))
    ((gtk4:widget-has-focus-p (gtk-widget (end-child parent))) (start-child parent))
    (t nil)))

;;(defgeneric active-view ((parent tabbed-layout)))

(defgeneric swap-layout-to (layout class &key &allow-other-keys)
  (:documentation "Sets Internal class to provided class-designator"))
  
(defmethod swap-layout-to :before ((layout single-layout) class &key &allow-other-keys)
  (gtk4:box-remove (inner-widget layout) (gtk-widget (child layout))))

(defmethod swap-layout-to :before ((layout paned-layout) class &key &allow-other-keys)
  (setf (gtk4:paned-start-child (inner-widget layout)) nil)
  (setf (gtk4:paned-end-child (inner-widget layout)) nil))

(defmethod swap-layout-to :before ((layout tabbed-layout) class &key &allow-other-keys)
  (iter (for child in (children layout))
    (gtk4:notebook-remove-page
     (inner-widget layout)
     (gtk4:notebook-get-page (inner-widget layout)))))


(defmethod swap-layout-to ((layout layout) (class (eql 'single-layout))
                           &key child)
  (change-class layout 'single-layout)
  (let ((inner-widget (gtk4:make-box :spacing 0 :orientation gtk4:+orientation-vertical+))
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


(defgeneric close-child (parent child)
  (:documentation "A child is requesting to be closed, remove the child if
  possible and turn into the appropriate layout."))

(defmethod close-child ((parent single-layout) child)
  (close-child (parent parent) parent))

(defmethod close-child ((parent paned-layout) child)
  (cond
    ((eq child (start-child parent))
     (swap-layout-to parent 'single-layout :child (end-child parent)))
    ((eq child (end-child parent))
     (swap-layout-to parent 'single-layout :child (start-child parent)))
    (t (message-error "attempting to remove child from parent widget, but that
  widget does not contain child"))))


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
       (swap-layout-to layout 'paned-layout
                       :start-child (if (or (eq :left layout-location) (eq :top layout-location))
                                        old-child
                                        child)
                       :end-child (if (or (eq :left layout-location) (eq :top layout-location))
                                        child
                                        old-child)
                       :orientation (if (or (eq :left layout-location) (eq :right layout-location))
                                        :horizontal
                                        :vertical)))
      (:none
       (if (transient-p old-child)
           (let ((wrapped-new-child (wrap-child-for layout child)))
             (setf (child layout) wrapped-new-child))
           (swap-layout-to layout 'paned-layout
                           :start-child old-child
                           :end-child child
                           :orientation :horizontal)))

      ((or :outside :centre)
       ;; convert to tabbed layout
       (message-error "layout-add-child-absolute ((single layout) :outside) not yet implemented")))
    layout))

(defmethod layout-add-child-absolute ((layout paned-layout) child &key (layout-location :none)) 
  (with-slots (gtk-widget inner-widget orientation start-child end-child) layout
    (match (cons orientation layout-location)
      ((cons :horizontal (or :bottom :top))
       (cond
         ((member :bottom (location-hints (start-child layout)))
          (message-error "Cannot yet call layout-add-child-absolute"))
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
                                              :parent layout
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
  (unless (gtk4:widget-vexpand-p (gtk-widget (view frame)))
    (setf (gtk4:widget-vexpand-p (gtk-widget modeline)) t))

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
                    (close-child (parent frame) frame)))
    (gtk4:box-append (gtk-widget modeline) close-btn)))

