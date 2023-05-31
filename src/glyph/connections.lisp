(in-package :glyph)

(defvar *gl-instances* nil)

(defclass glyph-connection (model)
  ((stream
    :accessor gl-stream
    :initarg :stream)
   (counter
    :accessor gl-counter
    :initform 0))
  (:documentation "Represents a (current) Glyph project/server"))


(defun make-listenting-thread ())

(defun make-connection (&key (host "localhost") (port 8801))
  (handler-case
      (let* ((socket (usocket:socket-connect host port
                                             :element-type '(unsigned-byte 8)))
             (stream (flex:make-flexi-stream
                      (usocket:socket-stream socket)
                      :external-format (flex:make-external-format :utf8)))
             (instance (make-instance 'gl-instance :stream stream)))
        ;(make-listening-thread instance)
        (push instance *gl-instances*)
        (print "connected")
        instance)
    (t (val) (format t "couln't connect!, error: ~A~%" val))))

(defclass connections-view (view) ())

(defmethod initialize-instance :after ((view connections-view) &key model)
  (let* ((widget (gtk4:make-box
                  :orientation gtk4:+orientation-vertical+
                  :spacing 0))
         (connections model)


         (list-model (gtk4:make-string-list :strings connections))
         (list-view (gtk4:make-list-view 
                     :model list-model
                     :factory (gtk4:make-signal-list-item-factory)))

         (add-connection-row (gtk4:make-box
                              :orientation gtk4:+orientation-horizontal+
                              :spacing 0))
         (hostname-text-box (gtk4:make-text))
         (port-text-box (gtk4:make-text))

         (add-connection-btn (gtk4:make-button :label "New")))

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
    (setf (view-widget view) widget)))



(defpackage glyph.rcp
  (:use :cl :iterate) ;json?
  (:import-from :glyph :gl-counter :gl-stream)
  (:export :run-code))
(in-package :glyph.rcp)

(defun run-code (connection code &key path)
  (json:encode-json-plist
   (:type "RunCode"
    :uid (incf (gl-counter))
    :path (if path path (list)))
   (gl-stream connection)))

(in-package :glyph)

