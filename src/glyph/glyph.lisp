(in-package :glyph)


(defclass glyph-view (text-view) ())


(defmethod initialize-instance :after ((view glyph-view) &key model)
  (let ((btn (gtk4:make-button
              :label "Run")))
    (gtk4:connect btn "clicked"
                  (lambda (btn)
                    (declare (ignore btn))
                    (when *gl-connections-model*
                      (run-code
                       (get-element 0 *gl-connections-model*)
                       (text-model-string (view-model view))))))
    (with-slots (modeline-widgets) view
      (push btn modeline-widgets))))


(defun hash-table-from-list (list)
  (iterate (for (key val) in list)
    (with table = (make-hash-table))
    (setf (gethash key table) val)
    (finally (return table))))


(defun list-connections (window)
  (window-add-view
   window
   (make-instance 'connections-view :model *gl-connections-model*)))


(defun new-playground (window)
  (window-add-view
   window
   (make-instance 'glyph-view
                  :model (make-instance 'text-model)
                  :input-mode +unicode-input-mode+)))


(defun show-server (window)
  (if (not (emptyp *gl-connections-model*))
      (window-add-view
       window
       (make-instance 'connection-view
                      :model (get-element 0 *gl-connections-model*)))
      (print "No server available")))

(defvar +menu-addons+
  (list))

(defparameter +glyph-plugin+
  (make-instance
   'plugin
   :name "Glyph"
   :about "A plugin for the Glyph Language"
   :commands (hash-table-from-list
              (list
               (list "glyph:connections" #'list-connections)
               (list "glyph:server" #'show-server)
               (list "glyph:playground" #'new-playground)))
   :views (list 'glyph-view)
   :menu-bar-submenus (list
                       (list "View"
                             (cons "connections" #'list-connections)
                             (cons "server" #'show-server)
                             (cons "playground" #'new-playground)))
   :models ()))


(defvar has-init nil)

(unless has-init
  (progn
    (setf has-init t)
    (register-plugin +glyph-plugin+)))
