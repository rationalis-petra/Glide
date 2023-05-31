(in-package :glyph)


(defclass glyph-view (glide:text-view) ())


(defun on-connect (widget user-data)
  (declare (ignore user-data))
  (let* (;; (root (gtk4:widget-root widget))
         (window (gtk4:make-dialog))
         (view (make-instance 'connections-view :model *gl-instances*)))
    ;; (setf (window-transient-for window) root)
    (setf (gtk4:window-child window) (view-widget view))
    (gtk4:window-present window)))


(defun initialize-glyph ()
  (setf (gethash "glyph:connect" glide:*commands*)
        (lambda (&key (host "localhost") (port 8801))
          (make-connection host port)))

  (let ((action (gio:make-simple-action
                 :name "glyph-connect"
                 :parameter-type nil)))
    (push (cons action #'on-connect) glide:*action-map*))

  (setf glide:*start-view* (make-instance 'glyph-view)))


(defun hash-table-from-list (list)
  (iterate (for (key val) in list)
    (with table = (make-hash-table))
    (setf (gethash key table) val)))


(defun connect-command ())


(defvar +glyph-plugin+
  (make-instance
   'glide:plugin
   :name "Glyph"
   :about "A plugin for the Glyph Language"
   :commands (hash-table-from-list
              '(("glyph:connect" #'connect-command)))
   :views (list 'glyph-view)
   :models ()))


(defvar has-init nil)


(unless has-init 
  (progn
    ; (register-plugin +glyph-plugin+)
    ; (pushnew #'initialize-glyph glide:*initializers*)
    (setf has-init t)))
