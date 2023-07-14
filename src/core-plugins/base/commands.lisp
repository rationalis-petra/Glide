;;;; commands.lisp

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

(in-package :glide/base)

(defun user-open-file (window)
  (ecase (setting '(style-settings dialog-preference))
    (:minibuffer
     (window-minibuffer-input
      window
      '(("file name:" . nil))
      (lambda (file-name)
        (window-add-view
         window
         (make-instance 'text-view
                        :model
                        (make-instance 'text-model
                                       :from (uiop:read-file-string file-name)
                                       :file (make-instance 'file :path file-name)))))))
    (:popup
     (gtk4:file-dialog-open
      (gtk4:make-file-dialog)
      (gtk-widget window)
      nil
      ;; TODO: replace this lambda with something appropriately async
      (lambda (file) (message-info (format nil "File was: ~A" file)))
      nil))))


(defun user-save-file (window)
  (let ((view (active-view window)))
    (cond
      ((and view (view-model view) (file (view-model view)))
       (save-file (file (view-model view)) (view-model view)))
      ((null view)
       (message-error "Unable to save file: no view in focus"))
      ((null (view-model view))
       (message-error "Unable to save file: no model for focus view"))
      (t (message-error "Unabled to save file: model has no file")))))

