;;;; defaults.lisp

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

;; Default values for various settings/customisation options


(setf
 *make-default-layout*
 (lambda (parent)
   (make-instance 'single-layout
                  :child (make-instance 'glide/base:dashboard-view)
                  :parent parent))

 *make-default-view* 
 (lambda () (make-instance 'glide/base:dashboard-view))

 *default-menu-desc*
 (list
  (list "Views"
        (cons "File Open" #'glide/base:user-open-file)
        (cons "File Save" #'glide/base:user-save-file)
        (cons "New" (lambda (window) (message-info "file new!"))))
  
  (list "Actions"
        (cons "Command"
              (lambda (window)
                (open-command-palette window))))
  (list "Help"
        (cons "About"
              (lambda (window)
                (declare (ignore window))
                (message-info "About")))
        (cons "Tutorial"
              (lambda (window)
                (declare (ignore window))
                (message-info "Tutorial")))
        (cons "Where is...?"
              (lambda (window)
                (declare (ignore window))
                (message-info "Where is...?"))))))

(define-settings-schema style-settings
  (theme
   (:name "Theme")
   (:type theme)
   (:selector (:multi-choice #'get-all-themes))
   (:default +explorer-dark-theme+))
  (dialog-preference
   (:name "Dialog Preference")
   (:type (or (eql :minibfufer) (eql :popup)))
   (:selector (:multi-choice (lambda () '(:minibuffer :popup))))
   (:default :minibuffer)))

;; (defvar *default-plugins*
;;   (list +base-plugin+ +glaze-plugin+ +glint-plugin+ +glyph-plugin+))
