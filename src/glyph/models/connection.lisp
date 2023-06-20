;;;; connection.lisp

;; Copyright (C) 2023 Connor Redfern
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :glyph)

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

(defvar *gl-connections-model* (make-instance 'list-model))


(defun make-connection (&key (host "localhost") (port 8801))
  (handler-case
      (let* ((socket (usocket:socket-connect host port
                                             :element-type '(unsigned-byte 8)))
             (stream (flex:make-flexi-stream
                      (usocket:socket-stream socket)
                      :external-format :utf8))
             (instance (make-instance 'glyph-connection
                                      :stream stream)))
        (add-element instance *gl-connections-model*)
        instance)
    (t (val) (format t "couldn't connect!, error: ~A~%" val))))

(defmethod initialize-instance :after ((conn glyph-connection) &key stream)
  (setf (listener-thread conn)
        (bt:make-thread (lambda () (recv-message conn)))))

(defun close-connection (connection))

(defun write-message (connection message)
  (gtk4:run-in-main-event-loop ()
      (glide:text-model-insert
       (repl-model connection)
       (glide:text-model-end-iter (repl-model connection))
       (format nil "~A~%" message))))

(defun write-error (connection message)
  (gtk4:run-in-main-event-loop ()
      (glide:text-model-insert
       (repl-model connection)
       (glide:text-model-end-iter (repl-model connection))
       (format nil "Error: ~A~%" message))))

;; Sending a message 
(defun eval-code (connection code &key (path (make-array 0)))
  (setf yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase) 
  (yason:encode-plist
   (list
    :type "EvalExpr"
    :uid (incf (server-counter connection))
    :path path
    :code code)
   (server-stream connection))
  (force-output (server-stream connection)))


(defun load-code (connection code &key (path (make-array 0)))
  (setf yason:*symbol-key-encoder* #'yason:encode-symbol-as-lowercase) 
  (yason:encode-plist
   (list
    :type "LoadModule"
    :uid (incf (server-counter connection))
    :path path
    :code code)
   (server-stream connection))
  (force-output (server-stream connection)))

;; Receiving a message
(defun recv-message (connection)
  (let ((message (yason:parse (server-stream connection))))
    (cond
      ((equal "quit" message) (close-connection connection))
      ((equal "Result" (gethash "type" message))
       (write-message connection (gethash "val" message))
       (recv-message connection))
      ((equal "Error" (gethash "type" message))
       (write-error connection (gethash "msg" message))
       (recv-message connection))
      (t
       ;; (yason:encode-plist message)
       ;; (yason:encode-plist)
       (recv-message connection)))))


