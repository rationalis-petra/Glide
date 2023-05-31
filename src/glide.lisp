(in-package #:glide)


(defun initialize (app)
  ;; blank state
  (setf *commands* (make-hash-table))
  (setf *action-map* nil)
  (setf *start-view* 'dashboard-view)

  ;; initialize 


  ;; initialize subsystems
  (mapcar #'funcall *initializers*)

  ;; use initialization data
  (iter (for (action . fun) in *action-map*)
    ;; TODO: change to window?
    (gio:action-map-add-action app action)
    (gtk4:connect action "activate" fun)))


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
