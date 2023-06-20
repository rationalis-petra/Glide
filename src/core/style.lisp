;;;; style.lisp

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

(defvar *current-style-provider* nil)

(defvar *default-theme* nil)

(defun set-widget-theme (widget theme))

;; (defun use-system-theme ())
;; (settings (gtk4:settings-default))

(defun set-app-theme (theme)
  (let ((style-provider
          (gtk4:css-provider-load-from-data
           style (lass:compile-and-write theme))))
    (gtk4:style-context-add-provider-for-display
     (gdk4:display-default)
     ;; (gdk4:display-manager-default-display
     ;;  (gdk4:display-manager-get)) 
     style
     gtk4:+style-provider-priority-application+)))

;; (defun set-app-theme (theme)
;;   (iter (for window in (gtk4:application-windows))
;;     (let ((child (gtk4:window-child window)))

;; ))) 
;; (defvar **)
