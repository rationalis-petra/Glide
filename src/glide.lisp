(in-package #:glide)


(defvar *commands*)
(defvar *menu-bar*)
(defvar *action-map*)


(defun build-gl-window (&key application title)
  (let* ((window (gtk4:make-application-window
                  :application application))
         (window-box (gtk4:make-box
                      :orientation +orientation-vertical+
                      :spacing 10))
         (menu-bar (gtk4:make-popover-menu-bar
                    :model *menu-bar*))
         (start-view (build-start-view)))

    (box-append window-box menu-bar)
    (box-append window-box start-view)

    (when title (setf (window-title window) title))
    (setf (window-child window) window-box)

    window))


(defun build-start-view ()
  (let* ((view-box (make-box :orientation +orientation-vertical+
                               :spacing 4))
         (text-view (make-text-view))
         (run-btn (make-button :label "Run"))

         (key-events (make-event-controller-key))
         (text-window (make-instance 'glide-window
                                :widget text-view
                                :input-mode +unicode-input-mode+)))
    (box-append view-box text-view)
    (box-append view-box run-btn)

    (connect key-events "key-pressed"
             (lambda (controller keyval keycode state)
               (declare (ignore controller state))
               (on-keypress text-window keyval keycode))
             :after text-view)

    (widget-add-controller text-view key-events)
    view-box))


(defun initialize (app)
  ;; blank state
  (setf *commands* (make-hash-table))
  (setf *menu-bar* (gio:make-menu))
  (setf *action-map* nil)

  ;; initialize subsystems
  (initialize-glyph)

  ;; use initialization data
  (iter (for (action . fun) in *action-map*)
    (gio:action-map-add-action app action)
    (connect action "activate" fun)))


(defun main ()
  (let ((app (make-application :application-id "org.rationalis-petra.glide"
                               :flags gio:+application-flags-flags-none+)))
    (connect app "activate"
             (lambda (app)
               (initialize app)
               (let ((window (build-gl-window
                              :application app
                              :title "Glide")))
                 (unless (widget-visible-p window)
                   (window-present window)))))
    (gio:application-run app nil)))


;; (defun main ()
;;   (initialize)
;;   (run))
