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

(defclass minibuffer (glide/base:text-view)
  ((enter-action
    :accessor enter-action
    :initform (lambda (minibuffer) (declare (ignore minibuffer))))))

(in-package :glide/base)

(defmethod initialize-instance :after ((minibuffer minibuffer) &key &allow-other-keys)
  (let* ((gtk-widget (gtk-widget minibuffer)))
    (setf (gtk4:widget-vexpand-p gtk-widget) nil)
    (setf (gtk4:text-view-editable-p gtk-widget) nil)
    (gtk4:widget-add-css-class gtk-widget "minibuffer")

    (flet ((show-info-message (message)
             (setf (text-model-string (view-model minibuffer)) message)
             (gtk4:widget-remove-css-class (gtk-widget minibuffer) "warning-text")
             (gtk4:widget-remove-css-class (gtk-widget minibuffer) "error-text"))
           (show-warning-message (message)
             (setf (text-model-string (view-model minibuffer)) message)
             (gtk4:widget-remove-css-class (gtk-widget minibuffer) "error-text")
             (gtk4:widget-add-css-class (gtk-widget minibuffer) "warning"))
           (show-error-message (message)
             (setf (text-model-string (view-model minibuffer)) message)
             (gtk4:widget-remove-css-class (gtk-widget minibuffer) "warning-text")
             (gtk4:widget-add-css-class (gtk-widget minibuffer) "error-text")))

      (glide::observe #'show-info-message glide::*info-mailbox*)
      (glide::observe #'show-warning-message glide::*warning-mailbox*)
      (glide::observe #'show-error-message glide::*error-mailbox*))

    (text-model-create-tag-type (view-model minibuffer) "locked" :editable nil)))

(defun window-minibuffer-input (window arglist callback)
  (let* ((minibuffer (minibuffer window))
         (buffer (gtk-buffer (view-model minibuffer))))
    (gtk4:widget-grab-focus (gtk-widget minibuffer))
    (iter (for (prompt . arg-type) in arglist)
      (setf (text-model-string (view-model minibuffer))
            (concatenate 'string prompt " "))
      (setf (gtk4:text-view-editable-p (gtk-widget minibuffer)) t)
      (let ((start (gtk4:text-buffer-start-iter buffer))
            (end (gtk4:text-buffer-end-iter buffer)))
        (gtk4:text-buffer-apply-tag-by-name buffer "locked" start end))
      (setf (enter-action minibuffer)
            (lambda (minibuffer)
              (let ((text (text-model-string (view-model minibuffer))))
                (funcall callback (subseq text (+ 1 (length prompt))))))))))

(defmethod on-keypress ((minibuffer minibuffer) keyval keystate)
  (if (= keyval gdk:+key-return+)
      (funcall (enter-action minibuffer) minibuffer)
      (call-next-method)))

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
   ))
   ;; :views (list 'glint-view)
   ;; :models (list 'glint-model)
   ;; :menu-bar-submenus (list (list "Views" (cons "Glint" #'glint-new)))

;; (defvar make-glyph-playground-layout () (make-vertical-layout ()))

(defvar has-init nil)
(unless has-init
  (progn
    ;; TODO: remove this!
    (setf has-init t)
    (register-plugin +base-plugin+)))
