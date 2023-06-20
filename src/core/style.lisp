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

;;; The style interface
;;
;; The style interface should provide theme makers with varying levels of
;; granularity, ranging from broad-strokes (font + colour palette) to 
;; very detailed (hand-written css/lass)

(in-package :glide)

(defvar *current-theme-style-provider* nil)

(defvar *default-theme* nil)

(defclass theme ()
  ((style-provider
    :documentation "The gtk style provider")))

(defgeneric get-style-provider (theme))

(defclass color-theme (theme)
  ((bg-primary)
   (bg-secondary)
   (fg-primary)
   (fg-secondary)))

(defmethod initialize-instance ((theme color-theme)
                                &key
                                  bg-primary
                                  bg-secondary
                                  fg-primary
                                  fg-secondary)
  (with-slots ((bp bg-primary) (bs bg-secondary) (fp fg-primary) (fs fg-secondary)) theme
    (setf bp bg-primary
          bs bg-secondary
          fp fg-primary
          fs fg-secondary)))

(defmethod get-style-provider ((theme color-theme))
  (let ((style-text
          (with-slots (bg-primary bg-secondary fg-primary fg-secondary) theme
            (apply
             #'lass:compile-and-write
             `(((:or window popover textview button listview)
               :color ,fg-primary
               :background-color ,bg-primary)

               (entry
                :color ,fg-primary
                :border-radius 0
                :background-color ,bg-secondary)

               (popover (contents
                         :color ,fg-primary
                         :background-color ,bg-primary))
               ((:or box entry)
                :border-style none)
               (textview
                :font-family "JuliaMono")
               ((:and .activatable :selected)
                :background-color ,bg-secondary)
               (button
                :background-image none
                :border-image none
                :background-color ,bg-secondary)
               (.modeline
                :border-style solid
                :border-width 2px 0px
                :border-color ,bg-secondary)))))
        (provider (gtk4:make-css-provider)))
    (gtk4:css-provider-load-from-data
       provider
       style-text)
    provider))

(defun set-app-theme (theme)
  (let ((style-provider (get-style-provider theme)))
    (when *current-theme-style-provider*
      (gtk4:style-context-remove-provider-for-display
       (gdk4:display-default)
       *current-theme-style-provider*))

    (gtk4:style-context-add-provider-for-display
     (gdk4:display-default)
     style-provider
     gtk4:+style-provider-priority-application+)

    (setf *current-theme-style-provider* style-provider)))

(defun compile-stylesheet (style)
  (apply #'lass:compile-and-write style))
