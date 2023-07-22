;;;; customise.lisp

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

;; This file defines several globals which are used as (default) customization
;; options, e.g. themes, default window layout, etc.

(defparameter +settings-directory+ nil)

(defvar *make-default-layout* nil)
(defvar *make-default-view* nil)
(defvar *default-menu-desc* nil)

(defclass settings-schema ()
  ((elements
    :type list
    :accessor elements 
    :initarg :elements
    :initform nil))
  (:documentation "A settings schema describes a particular group or set of
  settings. This includes the name (symbol) and type of each setting.  For
  example, a spellcheck plugin might store as settings preferred style for
  words, whether to spellcheck in comments, what backend to use etc."))

(defclass settings-value-schema ()
  ((lisp-name
    :type symbol
    :initarg :lisp-name)
   (type
    :type t
    :initarg :type)
   (default
    :initarg :default)
   ;; used in the UI
   (selector
    :reader selector
    :initarg :selector)
   (name
    :type string
    :reader name
    :initarg :name))
  (:documentation "A settings value schema describes a specific setting, like
  a color theme, "))

(defmethod name ((symbol symbol))
  (extra.string:symbol-pretty-name symbol))

(declaim (ftype (function (cons hash-table) hash-table) insert-hash))
(defun insert-hash (val hashtable)
  (setf (gethash (car val) hashtable) (cdr val))
  hashtable)

(declaim (ftype (function (settings-schema) hash-table) schema))
(defun make-default-settings (schema)
  (if (elements schema)
      (iter (for (name . child) in (elements schema))
        (typecase child
          (settings-schema
           (accumulate (cons name (make-default-settings child))
                       by #'insert-hash
                       initial-value (make-hash-table)))
          (settings-value-schema
           (accumulate (cons name (slot-value child 'default))
                       by #'insert-hash
                       initial-value (make-hash-table)))))
      (make-hash-table)))

(defvar *app-settings-schema* (make-instance 'settings-schema))
(defvar *app-settings* (make-default-settings *app-settings-schema*))

(defmacro define-settings-schema (name &body body)
  (flet ((make-val-schema (body)
           (with-prop-values (name type selector default) (cdr body)
             `(cons
               (quote ,(car body))
               (make-instance 'settings-value-schema
                              :lisp-name (quote ,(car body))
                              :type (quote ,type)
                              :default ,default
                              :selector (list ,(car selector) ,(cadr selector))
                              :name ,name)))))

    (let ((schema-sym (gensym "new-schema")))
      `(let ((,schema-sym
               (make-instance 'settings-schema
                              :elements
                              ,(cons 'list
                                     (mapcar #'make-val-schema body)))))
         (push (cons (quote ,name) ,schema-sym) (elements *app-settings-schema*))
         (setf (gethash (quote ,name) *app-settings*) (make-default-settings ,schema-sym))))))


(defun load-settings (settings-dir)
  (load (merge-pathnames #p"early-init" settings-dir)
        :if-does-not-exist nil))


(defun setting (path)
  (let ((setting *app-settings*))
    (iter (for key in path)
      (setf setting (gethash key setting)))
    setting))

;; TODO: 
(defun (setf setting) (val path)
  (labels ((update (val path settings)
             (match path
               ((list name)
                (setf (gethash name settings) val))
               ((cons hd tl)
                (update val tl (gethash hd settings))))))
    (update val path *app-settings*)))



;; used to create schema
;; (defmacro defsettings-schema)

;; used to create settings
;; (defmacro defsettings-schema)
