;;;; bridge.lisp

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

;; A bridge is an object which links two models together, keeping them in sync
;; Whenever the source object is updated, one of `bridge-update' or
;; `bridge-delta-update' should be invoked, to keep the destination model
;; up-tp-date.

(defclass bridge ()
  ((source
    :accessor source
    :initarg :source
    :type model)
   (destination
    :accessor destination
    :initarg :destination
    :type model))
  (:documentation "A bridge is an object which links together two models"))

;; Signalling

(defgeneric bridge-update (bridge)
  (:documentation "Invoked when a bridge's source object receives an update"))

(defgeneric bridge-delta-update (bridge delta)
  (:documentation "Invoked when a bridge's source object receives an update"))

;; Interaction with Models
(defgeneric make-bridge-source (model bridge)
  (:method ((model model) (bridge bridge))
    (setf (source bridge) model)
    (add-source model bridge)))

(defgeneric make-bridge-dest (model bridge)
  (:method ((model model) (bridge bridge))
    (setf (destination bridge) model)
    (add-dest model bridge)))


(defmethod initialize-instance ((bridge bridge) &key source destination)
  (make-bridge-source source bridge)
  (make-bridge-dest destination bridge))
