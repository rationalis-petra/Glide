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

(in-package :glint)

;; glint is a package for interacting with the glint document format,
;; a type of markup language.  
;; 
;; primary features include parsing and rendering to html.
;; 

(defun glint-new (window)
  (window-add-view
   window
   (make-instance 'glint-view
                  :model (make-default-doc))))

(defun split-glint-preview (window)
  (let ((view (active-view window)))
    (when (typep view 'text-view)
      (let* ((source (view-model view))
             (dest (make-instance 'glint-model))
             (bridge
               (make-instance 'text↦doc
                              :source source
                              :destination dest)))
        (bridge-update bridge)
        (window-add-view window (make-instance 'glint-view :model dest))))))


(defvar +glint-plugin+
  (make-instance
   'plugin
   :name "Glint"
   :about "A plugin for the Glint document Language"
   :views (list 'glint-view)
   :models (list 'glint-model)
   :commands (plugin-commands
              (command-group :glint
               (:when t)
               (:elements
                (command :split-glint-previentw
                         (:function #'split-glint-preview)
                         (:title "Glint Preview")))))

   :menu-bar-submenus (list
                       (list "Views" ;; TODO: when!!
                             (cons "Glint" #'glint-new))
                       (list "Actions" ;; TODO: add a when!!
                             (cons "Glint Preview (html)" #'split-glint-preview)))))

;; (defvar make-glyph-playground-layout () (make-vertical-layout ()))

(defvar has-init nil)
(unless has-init
  (progn
    ;; TODO: remove this!
    (setf has-init t)
    ;; (make-connection)
    ;; (setf *make-start-layout* #'make-glyph-playground-layout)

    (register-plugin +glint-plugin+)))
