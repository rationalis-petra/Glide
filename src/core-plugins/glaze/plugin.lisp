;;;; plugin.lisp

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

(in-package :glaze)

;; glaze is a package for interacting with the glaze document format,
;; a type of markup language.  
;; 
;; primary features include parsing and rendering to html.
;; 

(defun glaze-new (window)
  (window-add-view
   window
   (make-instance 'glyph-view
                  :model (make-default-doc))))


(defvar +glaze-plugin+
  (make-instance
   'plugin
   :name "Glaze"
   :about "A plugin for the Glyph document Language"
   :views (list 'glaze-view)
   :models (list 'glaze-model)

   :menu-bar-submenus (list
                       (list "Views" ;; TODO: when!!
                             (cons "Glaze" #'glaze-new)))))

;; (defvar make-glyph-playground-layout () (make-vertical-layout ()))

(defvar has-init nil)
(unless has-init
  (progn
    ;; TODO: remove this!
    (setf has-init t)
    ;; (make-connection)
    ;; (setf *make-start-layout* #'make-glyph-playground-layout)

    (register-plugin +glaze-plugin+)))
