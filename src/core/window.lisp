(in-package :glide)


;; TODO: frames contain views, we can have two frames adjacent, frames can swap
;; what view they contain.
      

(defclass window ()
  ((focus-view
    :accessor window-focus-view)
   (gtk-window
    :reader gtk-window)
   (palette
    :reader palette)
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
   (layout-parent)
   (shortcut-controller)))


(defgeneric (setf layout) (window layout))

;; layout

(defun run-command (window text)
  (setf (gtk4:widget-visible-p (palette window)) nil)
  (let ((result (gethash text *commands*)))
    (when result (funcall result window))))

(defun open-command-palette (window)
  (setf (gtk4:widget-visible-p (palette window)) t)
  (gtk4:widget-grab-focus (palette window)))


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
    (setf (slot-value window 'layout-widget) overlay)


    ;; Window command palette
    (let ((palette (gtk4:make-entry)))
      (setf (gtk4:widget-halign palette) gtk4:+align-center+)
      (gtk4:connect palette "activate"
                    (lambda (widget)
                      (let ((text (gtk4:entry-buffer-text (gtk4:entry-buffer widget))))
                        (run-command window text))))
      (gtk4:overlay-add-overlay overlay palette)
      (setf (gtk4:widget-visible-p palette) nil)
      (setf (slot-value window 'palette) palette))
    (setf (slot-value window 'layout-parent) overlay)))


(defmethod (setf window-layout) (new-layout (window window))
  ;; TODO: march down window layout & build hbox/vbox hierarchy
  (with-slots (layout layout-parent) window
    (setf (gtk4:overlay-child overlay) (gtk-widget new-layout))
    (setf layout new-layout)))


