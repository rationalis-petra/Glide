(in-package :glyph)


(defclass glyph-view (glide:text-view) ())


;; (defun on-connect (widget user-data)
;;   (declare (ignore user-data))
;;   (let* (;; (root (gtk4:widget-root widget))
;;          (window (gtk4:make-dialog))
;;          (view (make-instance 'connections-view :model *gl-connections*)))
;;     ;; (setf (window-transient-for window) root)
;;     (setf (gtk4:window-child window) (gtk-widget view))
;;     (gtk4:window-present window)))


(defun hash-table-from-list (list)
  (iterate (for (key val) in list)
    (with table = (make-hash-table))
    (setf (gethash key table) val)
    (finally (return table))))


(defun list-connections (window)
  (glide:window-add-view
   window
   (make-instance 'connections-view :model *gl-connections*)))

(defun new-playground (window)
  (glide:window-add-view
   window
   (make-instance 'glide:text-view
                  :model (make-instance 'glide:text-model)
                  :input-mode glide:+unicode-input-mode+)))

(defparameter +glyph-plugin+
  (make-instance
   'glide:plugin
   :name "Glyph"
   :about "A plugin for the Glyph Language"
   :commands (hash-table-from-list
              (list
               (list "glyph:connections" #'list-connections)
               (list "glyph:new-playground" #'new-playground)))
   :views (list 'glyph-view)
   :models ()))


(defvar has-init nil)

(unless has-init 
  (progn
    (glide:register-plugin +glyph-plugin+)))
