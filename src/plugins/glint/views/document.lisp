;;;; document.lisp

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

(in-package :glint)


(defclass glint-view (view) ())

(defgeneric make-css-style (theme)
  (:method ((theme color-theme))
    (with-slots (fg-primary
                 bg-primary

                 fg-secondary
                 bg-secondary

                 fg-muted
                 bg-muted

                 fg-info
                 fg-warning
                 fg-error) theme
      (apply
       #'lass:compile-and-write
       `((body
          :color ,fg-primary
          :background-color ,bg-primary
          ))))))


(defmethod initialize-instance :after ((view glint-view) &key model)
  (declare (ignore model))
  (with-slots (gtk-widget model) view
    (setf gtk-widget (webkit:make-web-view))
    (setf (gtk4:widget-vexpand-p gtk-widget) t)
    (setf (gtk4:widget-hexpand-p gtk-widget) t)
    (webkit:web-view-load-html gtk-widget 
     (render-html-body (render model :html)
                       :style (make-css-style (setting '(style-settings theme))))
     nil)))

(defmethod model-updated ((view glint-view))
  (with-slots (gtk-widget model) view
    (webkit:web-view-load-html
     gtk-widget
     (render-html-body (render model :html)
                       :style (make-css-style (setting '(style-settings theme))))
     nil)))

;; webkit not available (temporary)
;; (defmethod initialize-instance :after ((view glint-view) &key model)
;;   (declare (ignore model))
;;   (with-slots (gtk-widget model) view
;;     (setf gtk-widget (gtk4:make-text-view))
;;     (setf (gtk4:widget-vexpand-p gtk-widget) t)
;;     (setf (gtk4:widget-hexpand-p gtk-widget) t)
;;     (setf (gtk4:text-buffer-text (gtk4:text-view-buffer gtk-widget))
;;           (render-html-body (render model :html)
;;                        :style (make-css-style (setting '(style-settings theme)))))))

;; (defmethod model-updated ((view glint-view))
;;   (with-slots (gtk-widget model) view
;;     (setf (gtk4:text-buffer-text (gtk4:text-view-buffer gtk-widget))
;;           (render-html-body (render model :html)
;;                             :style (make-css-style (setting '(style-settings theme)))))))

;; webkit not available (when silc is working)
;; (defmethod initialize-instance :after ((view glint-view) &key model)
;;   (declare (ignore model))
;;   (with-slots (gtk-widget model) view
;;     (let ((silc-widget (render model :silc))) 
;;       (setf gtk-widget (reify silc-widget silc:gtk-backend))

;;       (setf (gtk4:widget-vexpand-p gtk-widget) t)
;;       (setf (gtk4:widget-hexpand-p gtk-widget) t))))

