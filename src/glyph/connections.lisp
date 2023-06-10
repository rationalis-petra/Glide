(in-package :glyph)


(defvar *gl-connections* nil)

(defclass glyph-connection (model)
  ((stream
    :accessor server-stream
    :initarg :stream
    :documentation "A network connection to the glyph server")
   (counter
    :accessor server-counter
    :initform 0
    :documentation "Used to give each message a unique id")
   (repl-model
    :accessor repl-model
    :initform (make-instance 'glide:text-model))
   (repl-lock
    :accessor repl-lock
    :initform (bt:make-lock))
   (listener-thread
    :accessor listener-thread
    :initarg :listener-thread))
  (:documentation "Represents a connection to a Glyph server"))

(defmethod initialize-instance :after ((conn glyph-connection) &key stream)
  (setf (listener-thread conn)
        (bt:make-thread (lambda () (recv-message conn)))))


(defun make-listenting-thread ())


(defun make-connection (&key (host "localhost") (port 8801))
  (handler-case
      (let* ((socket (usocket:socket-connect host port
                                             :element-type '(unsigned-byte 8)))
             (stream (flex:make-flexi-stream
                      (usocket:socket-stream socket)
                      :external-format :utf8))
             (instance (make-instance 'glyph-connection
                                      :stream stream)))
        (push instance *gl-connections*)
        (print "connected")
        instance)
    (t (val) (format t "couln't connect!, error: ~A~%" val))))


(defclass connections-view (view) ())


(defmethod initialize-instance :after ((view connections-view) &key model)
  (let* ((widget (gtk4:make-box
                  :orientation gtk4:+orientation-vertical+
                  :spacing 0))
         (connections model)


         (list-model (gtk4:make-string-list :strings nil))
         (list-view (gtk4:make-list-view 
                     :model list-model
                     :factory (gtk4:make-signal-list-item-factory)))

         (add-connection-row (gtk4:make-box
                              :orientation gtk4:+orientation-horizontal+
                              :spacing 0))
         (hostname-text-box (gtk4:make-entry))
         (port-text-box (gtk4:make-entry))

         (add-connection-btn (gtk4:make-button :label "New")))

    (flet ((new-connection (button)
             (declare (ignore button))
             (let* ((host-text (gtk4:entry-buffer-text (gtk4:entry-buffer
                                                        hostname-text-box)))
                    (port-text (gtk4:entry-buffer-text (gtk4:entry-buffer port-text-box))))
             (make-connection
              :host host-text
              :port (parse-integer port-text)))))
      (gtk4:connect add-connection-btn "clicked" #'new-connection))

    (gir:invoke ((gtk4:text-buffer hostname-text-box) 'set-text)
                (string "localhost") (length "localhost"))
    (gir:invoke ((gtk4:text-buffer port-text-box) 'set-text)
                (string "8801") (length "8801"))

    (gtk4:box-append add-connection-row hostname-text-box)
    (gtk4:box-append add-connection-row port-text-box)
    (gtk4:box-append add-connection-row add-connection-btn)

    ; (gtk4:box-append widget list)
    (gtk4:box-append widget add-connection-row)

    ; (gtk4:connect)
    (setf (gtk-widget view) widget)))


(defclass connection-view (view) ())


(defmethod initialize-instance :after ((view connection-view) &key model)
  (let* ((box (gtk4:make-box :orientation gtk4:+orientation-vertical+
                             :spacing 0))
         (repl-view (gtk4:make-text-view
                     :buffer (glide:gtk-buffer (repl-model model)))))
    (gtk4:box-append box repl-view)
    (setf (gtk-widget view) box)))

; (run-code (car *gl-connections*) "Hello, world") 
; (setf *gl-connections* nil) *gl-connections* 
; (force-output *standard-output*) 

;; Sending a message 
(defun run-code (connection code &key (path (make-array 0)))
  (format t "running code: ~A~%" code)
  (setf yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase) 
  (yason:encode-plist
   (list
    :type "RunCode"
    :uid (incf (server-counter connection))
    :path path
    :code code)
   (server-stream connection))
  (force-output (server-stream connection)))

;; Receiving a message
(defun recv-message (connection)
  (let ((message (yason:parse (server-stream connection))))
    (unless (equal "quit" message)
      (format t "received a message")
      (bt:with-lock-held ((repl-lock connection))
        (glide:text-model-insert
         (repl-model connection)
         (glide:text-model-end-iter (repl-model connection))
         (format nil "message: ~A" message)))
      (recv-message connection))))
