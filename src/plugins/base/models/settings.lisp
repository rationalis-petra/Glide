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

;; The settings model simple wraps a settings object defined in core. It is not
;; very interesting... 

;; TODO: change parent to object-model!
(defclass settings-model (model)
  ((settings-schema
    :type settings-schema
    :initarg :schema
    :accessor settings-schema
    :documentation "A settings schema is like a type descriptor for a settings
object, containing extra information like the default value for a setting, and
how to build a UI element to update it.")

   (settings-object
    :type hashmap
    :initarg :values
    :accessor settings-object
    :documentation "")))


;; The settings model simple wraps a settings object defined in core. It is not
;; very interesting... 
