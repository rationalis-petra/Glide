;;;; connections.lisp

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


(defvar *gl-connections-model* (make-instance 'list-model))


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
        (add-element instance *gl-connections-model*)
        instance)
    (t (val) (format t "couldn't connect!, error: ~A~%" val))))


(defclass connections-view (view) ())


(defclass connections-list-view (abstract-list-view) ())


(defmethod make-list-item-widget ((view connections-list-view))
  (gtk4:make-label :str "Connection"))


(defmethod update-list-item-widget ((view connections-list-view) connection widget))


(defmethod initialize-instance :after ((view connections-view) &key model)
  (let* ((widget (gtk4:make-box
                  :orientation gtk4:+orientation-vertical+
                  :spacing 0))
         (list-view (make-instance 'connections-list-view :model model))
         (connections model)

         (add-connection-row (gtk4:make-box
                              :orientation gtk4:+orientation-horizontal+
                              :spacing 0))
         (hostname-text-box (gtk4:make-entry))
         (port-text-box (gtk4:make-entry))

         (add-connection-btn (gtk4:make-button :label "New"))
         (clear-connection-btn (gtk4:make-button :label "Clear")))

    (flet ((new-connection (button)
             (declare (ignore button))
             (let* ((host-text (gtk4:entry-buffer-text (gtk4:entry-buffer
                                                        hostname-text-box)))
                    (port-text (gtk4:entry-buffer-text (gtk4:entry-buffer port-text-box))))
             (make-connection
              :host host-text
              :port (parse-integer port-text))))
           (clear-connections (button)
             (declare (ignore button))
             (clear *gl-connections-model*)))
      (gtk4:connect add-connection-btn "clicked" #'new-connection)
      (gtk4:connect clear-connection-btn "clicked" #'clear-connections))

    (gir:invoke ((gtk4:text-buffer hostname-text-box) 'set-text)
                (string "localhost") (length "localhost"))
    (gir:invoke ((gtk4:text-buffer port-text-box) 'set-text)
                (string "8801") (length "8801"))

    (gtk4:box-append add-connection-row hostname-text-box)
    (gtk4:box-append add-connection-row port-text-box)
    (gtk4:box-append add-connection-row add-connection-btn)
    (gtk4:box-append add-connection-row clear-connection-btn)

    (gtk4:box-append widget (gtk-widget list-view))
    (gtk4:box-append widget add-connection-row)

    (setf (gtk-widget view) widget)))


(defclass connection-view (view) ())


(defmethod initialize-instance :after ((view connection-view) &key model)
  (let* ((box (gtk4:make-box :orientation gtk4:+orientation-vertical+
                             :spacing 0))
         (repl-view (gtk4:make-text-view
                     :buffer (glide:gtk-buffer (repl-model model)))))

    ;; Set style
    (let ((style (gtk4:make-css-provider)))
      (gtk4:css-provider-load-from-data style
                                        "textview { font-family: JuliaMono; font-size: 10pt; }")
      (gtk4:style-context-add-provider (gtk4:widget-style-context repl-view) style glib:+maxuint32+))

    (gtk4:box-append box repl-view)
    (setf (gtk-widget view) box)))


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
(defun run-code (connection code &key (path (make-array 0)))
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
