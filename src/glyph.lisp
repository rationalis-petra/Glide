(in-package :glide)


(defvar *gl-instances* nil)

(defclass gl-instance ()
  ((stream
    :accessor gl-stream
    :initarg :stream))
  (:documentation "Represents a (current) Glyph project/server"))


(defun make-connection (&key (host "localhost") (port 8801))
  (let* ((socket (usocket:socket-connect host port
                                         :element-type '(unsigned-byte 8)))
         (stream (flex:make-flexi-stream
                  (usocket:socket-stream socket)
                  :external-format (flex:make-external-format :utf8)))
         (instance (make-instance 'gl-instance :stream stream)))
    (push instance *gl-instances*)
    instance))



(defun on-connect (action param)
  (declare (ignore action param))
  (make-connection)
  (print "connected"))

(defun initialize-glyph ()
  (setf (gethash "glyph:connect" *commands*)
        (lambda (&key (host "localhost") (port 8801))
          (make-connection host port)))

  (let ((submenu (gio:make-menu)))
    (gio:menu-append-item submenu (gio:make-menu-item
                                   :model *menu-bar*
                                   :label "connect"
                                   :detailed-action "app.glyph-connect"))
    (gio:menu-append-submenu *menu-bar* "Glyph" submenu))

  (let ((action (gio:make-simple-action
                 :name "glyph-connect"
                 :parameter-type nil)))
    (push (cons action #'on-connect) *action-map*)))


