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

(defvar *current-theme* nil)
(defvar *current-theme-style-provider* nil)

(defclass theme ()
  ((style-provider
    :documentation "The gtk style provider")))

(defgeneric get-style-provider (theme))

(defclass color-theme (theme)
  ((fg-primary)
   (bg-primary)

   (fg-secondary)
   (bg-secondary)

   (fg-muted)
   (bg-muted)

   (fg-info)
   (fg-warning)
   (fg-error)))

(defmethod initialize-instance ((theme color-theme)
                                &key
                                  bg-primary
                                  bg-secondary
                                   fg-primary
                                  fg-secondary
                                  fg-muted
                                  bg-muted
                                  fg-warning
                                  fg-error)
  (with-slots ((bp bg-primary) (bs bg-secondary)
               (fp fg-primary) (fs fg-secondary)
               (fm fg-muted)   (bm bg-muted)
               (fw fg-warning)  (fe fg-error)) theme
    (setf bp bg-primary
          bs bg-secondary
          fp fg-primary
          fs fg-secondary
          fm fg-muted
          bm bg-muted
          fw fg-warning
          fe fg-error)))

(defmethod get-style-provider ((theme color-theme))
  (let ((style-text
          (with-slots (fg-primary   bg-primary
                       fg-secondary bg-secondary
                       fg-muted     bg-muted
                       fg-warning   fg-error) theme
            (apply
             #'lass:compile-and-write
             `(((:or window popover
                     textview button
                     listview notebook
                     stack)
               :color ,fg-primary
               :background-color ,bg-primary)

               (headerbar
                :color ,bg-primary
                :background-image none
                :background-color ,fg-primary)
               ((headerbar button)
                :color ,bg-primary
                :background-color ,fg-primary)
               ((:and tab :checked)
                :color ,fg-primary
                :border none
                :background-color ,bg-primary)
               ((:or tabs tab header.top)
                :color ,fg-muted
                :border none
                :background-color ,bg-muted)
               (notebook
                :border-style none)

               (entry
                :color ,fg-muted
                :background-color ,bg-muted
                :border-radius 0)

               (popover (contents
                         :color ,fg-primary
                         :background-color ,bg-primary))
               ((:or box entry)
                :border-style none)
               (textview
                :font-family "JuliaMono")
               (separator
                :background-image ,(format nil "linear-gradient(~A, ~A)"
                                           bg-secondary
                                           bg-secondary))
               ((:and .activatable :selected)
                :background-color ,bg-secondary)
               (button
                :background-image none
                :border-image none
                :background-color ,bg-secondary)
               (.modeline
                :border-style solid
                :border-width 2px 0px
                :border-color ,bg-secondary)
               (.minibuffer
                :font-size 12pt
                :padding 4px 0px)
               (.stylish-text
                :font-family "Anurati")
               (.title-text
                :font-size 30pt)
               (.warning-text
                :color ,fg-warning)
               (.error-text
                :color ,fg-error)))))
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
