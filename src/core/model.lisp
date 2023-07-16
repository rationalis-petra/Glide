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
    :initarg :file
    :accessor file
    :initform nil
    :documentation "The file that was used to load this model (can be nil)")
   (source-for
    :initform nil)
   (dest-for
    :initform nil)
   (views
    :initform nil))
  (:documentation
   "A model subclass represents a data model"))

(defgeneric update-notify (model)
  (:method ((model model))
    (iter (for bridge in (slot-value model 'source-for))
      (bridge-update bridge))
    (iter (for view in (slot-value model 'views))
      (model-updated view))))

;; File formats can be used in two different ways:
;; • We have a model (with no corresponding file) and want to save it to a file
;; • We have a file (with particular format) and want to load a model from it
(defclass file-format ()
  (;; (extensions
   ;;  :accessor extensions
   ;;  :initarg :extensions)
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

(declaim (ftype (function (file) t) save-file))
(defgeneric save-file (file-info model)
  (:method (file-info (model model))
           ;; TODO: check if file info is empty 
           (message-error (format nil "Cannot save model: ~A" model))))

(defun add-source (model bridge)
   (push bridge (slot-value model 'source-for)))

(defun add-dest (model bridge)
   (push bridge (slot-value model 'dest-for)))
