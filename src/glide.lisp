(in-package #:glide)


(defun initialize (app)
  ;; Make sure state is reset 
  (setf *commands* (make-hash-table :test #'equal))
  (setf *start-view* 'dashboard-view)

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
                 (unless (gtk4:widget-visible-p (gtk-window window))
                   (gtk4:window-present (gtk-window window))))))

    (unwind-protect
         (gio:application-run app nil)
      (progn
        (mapcar (alexandria:compose
                 #'gtk4:window-close
                 (alexandria:rcurry #'gobj:pointer-object 'window))
                (glib:glist-list (gtk4:application-windows app)))
        (glib:idle-add (lambda () (gio:application-quit app)))))))

;; (gtk4:define-application (:name glide-main
;;                           :id "org.rationalis-petra.glide-main")
;;   (let ((window (make-instance 'window :app gtk4:*application* :title "Glide")))
;;     (gtk4:define-main-window (gtk-window (gtk-window window))
;;         (unless (gtk4:widget-visible-p gtk-window)
;;           (gtk4:window-present gtk-window)))))
