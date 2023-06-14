;;;; model.lisp

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

(defclass model ()
  ()
  (:documentation
   "A model subclass represents a data model"))


(defclass file-format ()
  ((extensions
    :accessor file-format-extensions)
   (loader
    :accessor file-format-loader)
   (saver
    :accessor file-format-saver)))

;; class generics
(defgeneric model-file-formats (model-class)
  (:method (model-class))
  (:documentation "File formats supported by a model"))

;; instance generics

;; (defgeneric model-file-formats (model-class) ())
