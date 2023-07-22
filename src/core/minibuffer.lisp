;;;; minibuffer.lisp

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

(defclass minibuffer ()
  ((text-buffer
    :accessor text-buffer)
   (text-widget
    :accessor text-widget)
   (gtk-widget
    :accessor gtk-widget)
   (enter-action
    :accessor enter-action
    :initform (lambda (minibuffer) (declare (ignore minibuffer))))
   (stash
    :accessor stash)))


(defmethod initialize-instance :after ((minibuffer minibuffer) &key &allow-other-keys)
  (setf (text-buffer minibuffer) (gtk4:make-text-buffer :table nil))
  (setf (gtk-widget minibuffer) (gtk4:make-text-view :buffer (text-buffer minibuffer)))
  ;(setf (text-buffer minibuffer) (gtk-widget minibuffer))

  (with-slots (text-buffer gtk-widget) minibuffer
    (setf (gtk4:widget-vexpand-p gtk-widget) nil)
    (setf (gtk4:text-view-editable-p gtk-widget) nil)
    (gtk4:widget-add-css-class gtk-widget "minibuffer")

    (flet ((show-info-message (message)
             (setf (gtk4:text-buffer-text text-buffer) message)
             (gtk4:widget-remove-css-class (gtk-widget minibuffer) "warning-text")
             (gtk4:widget-remove-css-class (gtk-widget minibuffer) "error-text"))
           (show-warning-message (message)
             (setf (gtk4:text-buffer-text text-buffer) message)
             (gtk4:widget-remove-css-class gtk-widget "error-text")
             (gtk4:widget-add-css-class gtk-widget "warning-text"))
           (show-error-message (message)
             (setf (gtk4:text-buffer-text text-buffer) message)
             (gtk4:widget-remove-css-class gtk-widget  "warning-text")
             (gtk4:widget-add-css-class gtk-widget "error-text")))

      (observe #'show-info-message *info-mailbox*)
      (observe #'show-warning-message *warning-mailbox*)
      (observe #'show-error-message *error-mailbox*))

    (let ((key-controller (gtk4:make-event-controller-key)))
      (gtk4:connect key-controller "key-pressed"
                    (lambda (controller keyval keycode state)
                      (declare (ignore controller keycode))
                      (on-keypress minibuffer keyval state)))
      (gtk4:widget-add-controller gtk-widget key-controller))

    (let ((tag (gtk4:make-text-tag :name "locked"))
          (tag-table (gtk4:text-buffer-tag-table text-buffer )))

      ;; TOOD: add more properties
      (setf (gir:property tag :editable) nil)

      (gtk4:text-tag-table-add tag-table tag))))


(defun window-minibuffer-input (window arglist callback)
  (let* ((minibuffer (minibuffer window))
         (buffer (text-buffer minibuffer)))
    (gtk4:widget-grab-focus (gtk-widget minibuffer))
    (gtk4:widget-remove-css-class (gtk-widget minibuffer) "warning-text")
    (gtk4:widget-remove-css-class (gtk-widget minibuffer) "error-text")

    (iter (for (prompt . arg-type) in arglist)
      (setf (gtk4:text-buffer-text buffer)
            (concatenate 'string prompt " "))
      (setf (gtk4:text-view-editable-p (gtk-widget minibuffer)) t)
      (let ((start (gtk4:text-buffer-start-iter buffer))
            (end (gtk4:text-buffer-end-iter buffer)))
        (gtk4:text-buffer-apply-tag-by-name buffer "locked" start end)
        (setf (enter-action minibuffer)
              (lambda (minibuffer)
                (let ((text (gtk4:text-buffer-text buffer)))
                  (gtk4:text-buffer-remove-tag-by-name buffer "locked" start end)
                  (setf (gtk4:text-view-editable-p (gtk-widget minibuffer)) nil)
                  (setf (gtk4:text-buffer-text buffer) "")
                  (funcall callback (subseq text (+ 1 (length prompt)))))))))))


(defmethod on-keypress ((minibuffer minibuffer) keyval keystate)
  (if (= keyval gdk:+key-return+)
      (funcall (enter-action minibuffer) minibuffer)))
