;;;; text.lisp

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


(defclass text-model (model)
  ((gtk-buffer
    :accessor gtk-buffer
    :initform (gtk4:make-text-buffer :table nil))
   (source-file
    :accessor source-file
    :initform nil))
  (:documentation "Wraps a GTK4 text buffer"))


;(defmethod initialize-instance ((model text-model) &key))

(declaim (ftype (function (text-model) string) text-model-string))
(defun text-model-string (model)
  "Get a string copy of the underlying text"
  (gtk4:text-buffer-text (gtk-buffer model)))

(defun (setf text-model-string) (string model)
  "Sets the underlying string of the text buffer"
  (setf (gtk4:text-buffer-text (gtk-buffer model)) string))

(declaim (ftype (function (text-model string) null) insert-at-cursor))
(defun insert-at-cursor (model string)
  (gir:invoke
   ((gtk-buffer model) :insert-at-cursor)
   string
   (utf-8-byte-length string))
  nil)

(declaim (ftype (function (text-model) t) text-model-end-iter))
(defun text-model-end-iter (model)
  (gtk4:text-buffer-end-iter (gtk-buffer model)))

(defun text-model-insert (model iterator string)
  (gir:invoke
   ((gtk-buffer model) :insert)
   iterator
   string
   (utf-8-byte-length string)))


;; (declaim (ftype (function (text-model &key (log-error t)) t) text-model-save))
;; (defun text-model-save (model)
;;   (with-slots (source-file) model
;;     (when source-file
;;       ())))
