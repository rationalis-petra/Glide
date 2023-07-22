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

(in-package :glide)


(in-package :glide/base)


(defvar +base-plugin+
  (make-instance
   'plugin
   :name "Glaze"
   :about "A plugin for the Glaze build tool"

   :commands (plugin-commands
              (command-group :base
               (:when t)
               (:elements
                (command :text-view
                         (:function #'new-text-view)
                         (:title "New Text View"))
                (command :open-file
                         (:function #'user-open-file)
                         (:title "Open File"))
                (command :save-file
                         (:function #'user-save-file)
                         (:title "Save File")))))
   :menu-bar-submenus
   (list
    (list "Settings"
          (cons "Application" #'open-settings-view)
          (cons "Window" (lambda (window) (message-info "window settings")))
          (cons "View" (lambda (window) (message-info "view settings")))))))

;; (defvar make-glyph-playground-layout () (make-vertical-layout ()))

(defvar has-init nil)
(unless has-init
  (progn
    ;; TODO: remove this!
    (setf has-init t)
    (register-plugin +base-plugin+)))
