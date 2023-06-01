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


(defun hash-table-from-list (list)
  (iterate (for (key val) in list)
    (with table = (make-hash-table))
    (setf (gethash key table) val)
    (finally (return table))))


(defun connect-command (window)
  (print "connected!")
  (finish-output))


(defvar +glyph-plugin+
  (make-instance
   'glide:plugin
   :name "Glyph"
   :about "A plugin for the Glyph Language"
   :commands (hash-table-from-list
              (list
               (list "glyph:connect" #'connect-command)))
   :views (list 'glyph-view)
   :models ()))


(defvar has-init nil)


(unless has-init 
  (progn
    (glide:register-plugin +glyph-plugin+)))
