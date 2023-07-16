;;;; package.lisp

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

(defpackage glide
  (:use :cl :extra.coll :iterate :trivia)
  (:export
   ;; main function, for easy running
   :main :glide-app

   ;; customization
   :setting
   :*make-default-layout*
   :deftheme
   :+explorer-theme+
   :style-settings
   :theme
   :dialog-preference

   ;; window stuf
   :window
   :window-add-view 
   :active-view
   :enter-action
   :minibuffer
   :window-minibuffer-input

   ;; models & file formats
   :model
   :add-source
   :add-dest
   :update-notify

   :file-format
   :model-file-formats
   :file
   :path

   ;; bridges
   :bridge
   :source :destination
   :bridge-update
   :bridge-delta-update
   :make-bridge-source
   :make-bridge-dest

   ;; views
   :view
   :view-model :gtk-widget :modeline-widgets :transient-p
   :model-updated
   :add-keymap

   ;; keys
   :keymap
   :defkeymap

   ;;;; Plugin Utilities
   :plugin :register-plugin

   ;; commands
   :plugin-commands :command :command-group

   ;; misc. API
   :message-info
   :message-warning
   :message-error))
(in-package :glide)

;; Global Variables 

