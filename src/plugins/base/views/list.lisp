;;;; list.lisp

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
                    (let* ((widget (gtk4:list-item-child item))
                           (item-name (gtk4:string-object-string
                                       (gobj:coerce (gtk4:list-item-item item)
                                                    'gtk4:string-object)))
                           (lisp-item (gethash item-name (lisp-vals model))))
                      (format t "item-name: ~A~%lisp-item: ~A~%" item-name lisp-item)
                      (update-list-item-widget view lisp-item widget))))
    (gtk4:connect factory "unbind"
                  (lambda (factory item)
                    (declare (ignore factory item)))) 
    (gtk4:connect factory "teardown"
                  (lambda (factory item)
                    (declare (ignore factory item))))))

;(defclass list-view (abstract-list-view))
