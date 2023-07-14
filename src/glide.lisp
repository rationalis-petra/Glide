;;;; glide.lisp

;(uiop:symbol-call :gir :require-namespace "WebKit2" "4.1")

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

(in-package #:glide)


(defun initialize (app)
  ;; Make sure state is reset 
  (setf +settings-directory+ 
        (cond 
          ((equal "Win32" (software-type))
           (merge-pathnames #p"AppData/Roaming/glide/" (user-homedir-pathname)))
          ((equal "Linux" (software-type))
           (merge-pathnames #p".config/glide/" (user-homedir-pathname)))
          (t
           (print "unable to locate config direcotry for your os!")
           (exit))))
  (load-settings +settings-directory+)
  ;; (setf *commands* (make-hash-table :test #'equal))
  (set-app-theme (setting '(style-settings theme)))

  ;; initialize plugins
  (mapcar #'initialize-plugin *plugins*)
  (mapcar #'load-plugin *plugins*))

(defun main ()
  (let ((app (gtk4:make-application
              :application-id "org.rationalis-petra.glide"
              :flags gio:+application-flags-flags-none+)))
    (gtk:connect app "activate"
             (lambda (app)
               (initialize app)
               (let ((window (make-instance 'window
                              :app app
                              :title "Glide")))
                 (unless (gtk4:widget-visible-p (gtk-widget window))
                   (gtk4:window-present (gtk-widget window))))))

    (unwind-protect
         (gio:application-run app nil)
      (progn
        (mapcar (∘
                 #'gtk4:window-close
                 (⟜ #'gobj:pointer-object 'window))
                (glib:glist-list (gtk4:application-windows app)))
        (glib:idle-add (lambda () (gio:application-quit app)))))))
