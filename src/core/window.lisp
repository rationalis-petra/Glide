(in-package :glide)


;; TODO: frames contain views, we can have two frames adjacent, frames can swap
;; what view they contain.
      

(defclass window ()
  ((focus-view
    :accessor window-focus-view
    :documentation "The current view that is")
   (gtk-window
    :reader gtk-window
    :documentation "The underlying gtk window")
   (palette
    :reader palette)
   (action-group
    :reader action-group
    :initform (gio:make-simple-action-group))
   (layout
    :reader window-layout
    :documentation "")
   (keymap
    :accessor window-keymap)

   ;; "internal" slots → do not form a public interface
   (layout-parent)
   (shortcut-controller
    :initform (gtk4:make-shortcut-controller))))


(defgeneric (setf layout) (window layout))


(defun run-command (window text)
  (setf (gtk4:widget-visible-p (palette window)) nil)
  ;(setf (gtk4:entry-buffer-text (palette window)) "")
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

(defun make-initial-menu-desc ()
  (reduce #'merge-menu-descs
          (append (list +default-menu-desc+) (get-menu-descs))))


;; Eagerly use menu-2's item
;; TODO: merge in any toplevel items occuring 
(defun merge-menu-descs (menu-1 menu-2)
  (flet ((merge-elements (name result child)
           (if result
               (cons name (merge-menu-descs child (cdr result)))
               (cons name child))))

    (cond
      ((not menu-1) menu-2)
      ((and (listp menu-1) (listp menu-2))
       (let ((menu-1-merge 
               (iter (for (name . child) in menu-1)
                 (let ((res (assoc name menu-2 :test #'equal)))
                   (collect (merge-elements name res child)))))
             (menu-2-merge
               (iter (for (name . child) in menu-2)
                          (unless (assoc name menu-1)
                            (collect (cons name child))))))
         (append menu-1-merge menu-2-merge)))

      (t menu-2))))


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
  (let* ((gtk-window (gtk4:make-application-window
                              :application app))
         (window-box (gtk4:make-box
                      :orientation gtk4:+orientation-vertical+
                      :spacing 10))
         (menu-bar (make-menu-bar window (make-initial-menu-desc)))
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
                :parameter-type nil)))
    (setf (slot-value window 'layout-parent) overlay)
    (setf (slot-value window 'gtk-window) gtk-window)

    ;; register action group + controller with window.
    (gtk4:widget-insert-action-group gtk-window "window" (action-group window))
    (gtk4:widget-add-controller gtk-window (slot-value window 'shortcut-controller))

    (gio:action-map-add-action (action-group window) command-palette-action)
    ;; shortcuts
    (gtk4:connect command-palette-action "activate"
                  (lambda (p h)
                    (declare (ignore p h))
                    (open-command-palette window)))

    (gtk4:shortcut-controller-add-shortcut
     (slot-value window 'shortcut-controller)
     command-palette-shortcut)


    ;; window construction
    (gtk4:box-append window-box menu-bar)
    (setf (window-layout window) (make-single-layout start-frame))
    (gtk4:box-append window-box overlay)

    (when title (setf (gtk4:window-title gtk-window) title))
    (setf (gtk4:window-child gtk-window) window-box)
    (gtk4:window-maximize gtk-window)


    ;; Window command palette
    (let ((palette (gtk4:make-entry)))
      (setf (gtk4:widget-halign palette) gtk4:+align-center+)
      (gtk4:connect palette "activate"
                    (lambda (widget)
                      (let ((text (gtk4:entry-buffer-text (gtk4:entry-buffer widget))))
                        (run-command window text))))
      (gtk4:overlay-add-overlay overlay palette)
      (setf (gtk4:widget-vexpand-p palette) nil)
      (setf (gtk4:widget-visible-p palette) nil)
      (setf (slot-value window 'palette) palette))))


(defmethod (setf window-layout) (new-layout (window window))
  ;; TODO: march down window layout & build hbox/vbox hierarchy
  (with-slots (layout layout-parent) window
    (setf (gtk4:overlay-child layout-parent) (gtk-widget new-layout))
    (setf layout new-layout)))



(defun window-add-view (window view)
  (layout-add-child
   (window-layout window)
   (make-instance 'frame :view view)
   :orientation :horizontal))
