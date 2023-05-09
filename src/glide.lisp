(in-package #:glide)




(defun run ()
  (let ((app (make-application :application-id "org.rationalis-petra.glide"
                               :flags gio:+application-flags-flags-none+)))
    (connect app "activate"
             (lambda (app)
               (let ((window (make-application-window :application app)))
                 (setf (window-title window) "Glide")
                 (build-start-window window)
                 (unless (widget-visible-p window)
                   (window-present window)))))
    (gio:application-run app nil)))

(defun build-start-window (window)
  (let* ((window-box (make-box :orientation +orientation-vertical+
                               :spacing 4))
         (text-view (make-text-view))
         (run-btn (make-button :label "Run"))

         (key-events (make-event-controller-key))
         (text-window (make-instance 'glide-window
                                :widget text-view
                                :input-mode +unicode-input-mode+)))
    (box-append window-box text-view)
    (box-append window-box run-btn)

    (connect key-events "key-pressed"
             (lambda (controller keyval keycode state)
               (declare (ignore controller state))
               (on-keypress text-window keyval keycode))
             :after text-view)

    (widget-add-controller text-view key-events)
    (setf (window-child window) window-box)))

