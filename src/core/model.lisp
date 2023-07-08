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
  ((file
    :accessor :file
    :initform nil
    :documentation ""))
  (:documentation
   "A model subclass represents a data model"))


;; File formats can be used in two different ways:
;; • We have a model (with no corresponding file) and want to save it to a file
;; • We have a file (with particular format) and want to load a model from it
(defclass file-format ()
  ((extensions
    :accessor extensions
    :initarg :extensions)
   (loader
    :accessor loader)
   (saver
    :accessor saver))
  (:documentation "A file format represents (as the name implies) a particular
  file format."))

(defclass file ()
  ((file-format
    :reader file-format
    :initarg :format
    :type file-format)
   (path
    :reader path
    :initarg :path
    :type pathname)))

;; class generics
(defgeneric model-file-formats (model-class)
  (:method (model-class))
  (:documentation "File formats supported by a model"))


(defvar *file-formats* nil)
;; instance generics

;; (defgeneric model-file-formats (model-class) ())
