;;;; reporting.lisp

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

(in-package :glide)

;; Here, we specify several functions (And globals) useful for having  
;; common lisp report information to the user (e.g. error/info messages,
;; logging, etc.)
;;
;; These functions are thread-safe 

(defclass mailbox ()
  ((history
    :initform t)
   (lock
    :reader lock
    :initform (bt:make-lock))
   (observers
    :accessor observers
    :initform (make-array 10 :fill-pointer 0)
    :type list)
   (messages
    :reader messages
    :initform (make-array 10 :fill-pointer 0)
    :type (array string))))

(defgeneric notify (observer message))

(defun observe (observer mailbox)
  (bt:with-lock-held ((lock mailbox))
    (vector-push-extend observer (observers mailbox))))

(defun send-message (message mailbox)
  (bt:with-lock-held ((lock mailbox))
    (vector-push-extend message (messages mailbox))
    (map 'vector (lambda (observer) (notify observer message)) (observers *info-mailbox*))))

(defvar *info-mailbox*    (make-instance 'mailbox))
(defvar *error-mailbox*   (make-instance 'mailbox))
(defvar *warning-mailbox* (make-instance 'mailbox))

(declaim (ftype (function (string) t) message-info))
(defun message-info (message)
  (send-message message *info-mailbox*))

(declaim (ftype (function (string) t) message-warning))
(defun message-warning (message)
  (send-message message *error-mailbox*))

(declaim (ftype (function (string) t) message-error))
(defun message-error (message)
  (send-message message *warning-mailbox*))
