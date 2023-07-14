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

(defvar *active-schemas* nil)
(defvar *app-settings* nil)

(defclass settings-schema () ()
  (:documentation "A settings schema describes a particular group or set of
  settings. This includes the name (symbol) and type of each setting.  For
  example, a spellcheck plugin might store as settings preferred style for
  words, whether to spellcheck in comments, what backend to use etc."))

(defmacro define-settings-schema (name &body body)
  `(progn
     (defclass ,name (settings-schema)
     ,(iter (for (name type default) in body)
        (collect `(,name :type ,type :initform ,default))))
     ;; (setf
     ;;  *active-schemas*
     ;;  (acons (quote ,name) (make-instance (quote ,name)) *active-schemas*))
     (setf
      *app-settings*
      (acons (quote ,name) (make-instance (quote ,name)) *active-schemas*))))


(defun load-settings (settings-dir)
  (load (merge-pathnames #p"early-init" settings-dir)
        :if-does-not-exist nil))


(defun setting (path)
  (let ((setting (cdr (assoc (car path) *app-settings*)))
        (new-path (cdr path)))
    (iter (for slot in new-path)
      (setf setting (slot-value setting slot)))
    setting))


(defun (setf setting) (val path)
  (let ((schema (cdr (assoc (car path) *app-settings*)))
        (final-slot (car (last path)))
        (new-path (butlast (cdr path) 1)))
    (iter (for slot in new-path)
      (setf schema (slot-value schema slot)))
    (setf (slot-value schema final-slot) val)))



;; used to create schema
;; (defmacro defsettings-schema)

;; used to create settings
;; (defmacro defsettings-schema)
