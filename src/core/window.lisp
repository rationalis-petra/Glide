(in-package :glide)


;; TODO: frames contain views, we can have two frames adjacent, frames can swap
;; what view they contain.
      

(defclass window ()
  ((focus-view
    :accessor window-focus-view)
   (gtk-window
    :reader gtk-window)
   (action-group
    :reader action-group
    :initform (gio:make-simple-action-group))
   (layout-widget
    :reader layout-widget)
   (layout
    :reader window-layout)
   ;; keymap/shortcuts
   (keymap
    :accessor window-keymap)

   ;; "internal" slots → do not form a public interface
   (shortcut-controller)))


(defgeneric (setf layout-widget) (window widget))

(defgeneric (setf layout) (window layout))

;; layout
(defclass frame ()
  ((current-view
    :initarg :view
    :accessor frame-view)
   (gtk-widget
    :reader gtk-widget)))


(defmethod initialize-instance :after ((frame frame) &key view)
  (let ((box (gtk4:make-box
              :orientation gtk4:+orientation-vertical+
              :spacing 10))
        (label (gtk4:make-label :str "modeline")))
    (gtk4:box-append box (view-widget view))
    (gtk4:box-append box label)
    
    (setf (slot-value frame 'gtk-widget) box)))

(defun run-command (window text)
  (format t "running command: ~A~%" text))

(defun open-command-palette (window)
  (with-slots (layout-widget) window
    (let ((palette (gtk4:make-entry)))
      (setf (gtk4:widget-halign palette) gtk4:+align-center+)
      (gtk4:connect palette "activate"
                    (lambda (widget)
                      (let ((text (gtk4:entry-buffer-text (gtk4:entry-buffer widget))))
                        (run-command window text))))
      (gtk4:overlay-add-overlay layout-widget palette))))


(defvar +default-menu-desc+
  (list
   (list "View"
         (cons "file Open" (lambda (window) (print "file opened!")))
         (cons "File Save" (lambda (window) (print "file saved!")))
         (cons "File New" (lambda (window) (print "file new!"))))
   (list "Settings"
         (cons "Application" (lambda (window) (print "application settings")))
         (cons "Window" (lambda (window) (print "window settings")))
         (cons "View" (lambda (window) (print "view settings"))))
   (list "Actions"
         (cons "Command"
               (lambda (window)
                 (open-command-palette window))))))


(defun make-menu-bar (window menu-items)
  (iter (for (subheading . submenu-items) in menu-items)
    (with menu = (gio:make-menu))
    (with action-group = (action-group window))
    (iter (for (name . fun) in submenu-items)
      (with submenu = (gio:make-menu))
      (let* ((action-name (string (gensym)))
             (item (gio:make-menu-item
                    :detailed-action
                    ;; TODO: remove window.??
                    (concatenate 'string "window." action-name)
                    :model menu
                    :label name))
             (action
               (gio:make-simple-action
                :name action-name
                :parameter-type nil)))
        (gio:menu-append-item submenu item)
        (gio:action-map-add-action action-group action)
        (gtk4:connect action "activate"
                      (lambda (w u)
                        (declare (ignore w u))
                        (funcall fun window))))
      (finally
       (gio:menu-append-submenu menu subheading submenu)))
    (finally
     
     (return (gtk4:make-popover-menu-bar :model menu)))))


(defmethod initialize-instance :after ((window window) &key app title)
  (let* ((gtk-window (if app (gtk4:make-application-window
                              :application app)))
         (window-box (gtk4:make-box
                      :orientation gtk4:+orientation-vertical+
                      :spacing 10))
         (menu-bar (make-menu-bar window +default-menu-desc+))
         (start-view (make-instance 'dashboard-view))
         (start-frame (make-instance 'frame :view start-view))

         (overlay (gtk4:make-overlay))

         (command-palette-shortcut
           (gtk4:make-shortcut
            :trigger (gtk4:shortcut-trigger-parse-string :string "<Ctrl>colon")
            :action (gtk4:shortcut-action-parse-string :string "action(window.command_palette)")))

         (command-palette-action
           (gio:make-simple-action
                :name "command_palette"
                :parameter-type nil))
         ;; (gtk4:make-callback-action
         ;;   :callback (lambda (w h) (open-command-palette window))
         ;;   :data nil
         ;;   :destroy nil)

         (shortcut-controller
           (gtk4:make-shortcut-controller)))

    (gtk4:widget-insert-action-group gtk-window "window" (action-group window))

    (gio:action-map-add-action (action-group window) command-palette-action)
    ;; shortcuts
    (gtk4:connect command-palette-action "activate"
                  (lambda (p h)
                    (declare (ignore p h))
                    (open-command-palette window)))

    (gtk4:shortcut-controller-add-shortcut
     shortcut-controller
     command-palette-shortcut)

    (gtk4:widget-add-controller gtk-window shortcut-controller)

    ;; window construction
    (gtk4:box-append window-box menu-bar)
    (setf (gtk4:overlay-child overlay) (gtk-widget start-frame))
    (gtk4:box-append window-box overlay)

    (when title (setf (gtk4:window-title gtk-window) title))
    (setf (gtk4:window-child gtk-window) window-box)
    (gtk4:window-maximize gtk-window)


    (setf (slot-value window 'gtk-window) gtk-window)
    (setf (slot-value window 'layout-widget) overlay)))


(defmethod (setf window-layout) (frame (window window))
  ;; TODO: march down window layout & build hbox/vbox hierarchy
  (with-slots (layout gtk-window) window
    (let ((overlay (gtk4:make-overlay)))
      (setf (gtk4:overlay-child overlay) frame)
      (setf layout frame)
      (setf (layout-widget window) overlay))))


(defmethod (setf layout-widget) (widget (window window))
  (let ((box (gtk4:window-child (gtk-window window))))
    (when (layout-widget window)
      (gtk4:box-remove box (layout-widget window)))
    (gtk4:box-append box widget)))