;;;; dashboard.lisp

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
