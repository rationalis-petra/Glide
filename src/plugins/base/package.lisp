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

(defpackage glide/base
  (:use :cl :iterate :glide :trivial-utf-8)
  (:export

   :text-view
   :+unicode-input-mode+
   :input-mode

   :dashboard-view

   :abstract-list-view
   :make-list-item-widget
   :update-list-item-widget

   ;;utility
   :user-save-file
   :user-open-file

   :text-model
   :gtk-buffer :text-model-string :text-model-insert :text-model-end-iter

   :list-model
   :emptyp
   :clear
   :get-element
   :add-element))
