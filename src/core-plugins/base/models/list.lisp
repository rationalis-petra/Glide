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

(defclass list-model (model)
  ((lisp-vals
    :accessor lisp-vals
    :initform (make-hash-table))
   (gtk-list
    :accessor gtk-list
    ;; :initform (gio:make-list-store :item-type (gobject:type-from-name "GValue"))
    :initform (gtk4:make-string-list :strings ())))
  (:documentation "Wraps a GTK4 list model. Elements are assumed to be CL values"))

(defun emptyp (model)
  (equal (hash-table-count (lisp-vals model)) 0))

(defun get-element (n model)
  ;; TODO: this function is dodgy as shit
  (declare (ignore n))
  (iter (for (key val) in-hashtable (lisp-vals model)) (return val)))

(defgeneric add-element (element model)
  (:documentation "Add ELEMENT into MODEL. ELEMENT is placed at the head of the list"))

(defgeneric clear (model)
  (:documentation "Remove all items from MODEL"))

(defgeneric pop-element (model)
  (:documentation "Remove the first element from MODEL"))

(defgeneric remove-element-if (predicate model)
  (:documentation "Remove all elements for which PREDICATE does not return nil"))

(defgeneric remove-element (element model &key test)
  (:documentation "Remove all elements e from MODEL for which (test ELEMENT e)
returns a generalized truth value"))

(defmethod add-element (element (model list-model))
    (let ((name (string (gensym))))
      (setf (gethash name (lisp-vals model)) element)
      (gio:list-store-append (gtk-list model) name)))

(defmethod clear ((model list-model))
  (setf (lisp-vals model) (make-hash-table))
  (gio:list-store-remove-all (gtk-list model)))
