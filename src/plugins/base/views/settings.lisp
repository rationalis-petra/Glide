;;;; settings.lisp

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

(in-package :glide/base)

;; (defclass settings-list-view (abstract-list-view) ())

;; (defmethod make-list-item-widget ((view settings-list-view))
;;   (gtk4:make-label :str "Connection"))

;; (defmethod update-list-item-widget ((view settings-list-view) name widget)
;;   (setf (gtk4:label-text (gobj:coerce widget 'gtk4:label)) (string name)))

(defclass settings-view (view)
  ((child-pages
    :type list
    :initform nil))
  (:documentation "Shows all (application-wide) settings"))

;; (defclass settings-schema-view (view) ()
;;   (:documentation ""))

;;(defmethod initialize-instance :after ((view settings-view) &key &allow-other-keys))
(defun make-selector (child)
  (match (glide::selector child)
    ((list :multi-choice func)
     (format t "result: ~A~%" (funcall func))
     (gtk4:make-drop-down :strings
                          (mapcar #'name (funcall func))))))

(defun make-settings-menu (schema values)
  (let ((box (gtk4:make-box :orientation gtk4:+orientation-vertical+
                            :spacing 10)))
    (iter (for (key . child) in (glide::elements schema))
          (for value = (gethash key values))
      (typecase child
        (settings-schema
         (gtk4:box-append box (make-settings-menu child value)))
        (settings-value-schema
         (let* ((new-widget (gtk4:make-box :orientation gtk4:+orientation-horizontal+
                                           :spacing 10))
                (name (gtk4:make-label :str (name child)))
                (selector (make-selector child)))
           (gtk4:box-append new-widget name)
           (gtk4:box-append new-widget selector)
           (gtk4:box-append box new-widget)))))
    box))


(defmethod initialize-instance :after ((view settings-view) &key &allow-other-keys)
  (with-slots (gtk-widget model child-pages) view
    (setf gtk-widget (gtk4:make-notebook))
    (iter (for (key . local-schema) in (glide::elements (settings-schema model)))
          (for value = (gethash key (settings-object model)))
          
      (let ((child-page (make-settings-menu local-schema value)))
        (push child-page child-pages)
        (gtk4:notebook-append-page
         gtk-widget
         child-page
         (gtk4:make-label :str (string key)))))))
