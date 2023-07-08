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

(defvar *default-theme* nil)
(defvar *make-default-layout* nil)
(defvar *make-default-view* nil)
(defvar *default-menu-desc* nil)

(defclass style-settings ()
  ((theme
    :type theme)))

(defclass settings ()
  ((default-layout)
   (style-settings
    :type style-settings)))

(defun load-settings (settings-dir)
  (load (merge-pathnames #p"early-init" settings-dir)
        :if-does-not-exist nil))

(defvar settings (make-instance 'settings))

;(defmacro defsettings)
