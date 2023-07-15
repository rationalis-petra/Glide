;;;; plugin.lisp

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
;; Example 1: a simple image viewer plugin 
;; (defplugin :image-viwer
;;   :views (image-view image-editor)
;;   :models (image)
;;   :init load-imagemagik)

;; Globals relatedto plugins
(defvar *plugins* nil)
(defvar *start-view* nil)
(defvar *text-commands* (make-hash-table :test #'equal))


(defun register-plugin (plugin)
  (pushnew plugin *plugins*))


(defun register-models (plugin models))


(defun register-views (plugin views)
"For PLUGIN, register the list of VIEWS. This means two things:
1. The list VIEWS is associated with the plugin.
2. Glide will look at the models associated with each VIEW, and make sure
   they are available for that model. Further, a given view may request to be
   the default view for a given datatype.")


(defun get-menu-descs ()
  (iter (for plugin in *plugins*)
    (collect (slot-value plugin 'menu-bar-submenus))))


(defclass plugin ()
  ((name
    :initarg :name
    :documentation
    "The name of the plugin")
   (about
    :initarg :about
    :documentation
    "Information about the plugin (name, author, version etc.)")
   (menu-bar-submenus
    :initarg :menu-bar-submenus
    :initform nil 
    :documentation
    "A List of submenus that are always present in the menu bar when this plugin
  is loaded.")
   (commands
    :initarg :commands
    :initform nil
    :documentation
    "A list of (named) commands that are always present when this plugin is loaded")
   (views
    :initarg :views
    :initform nil 
    :documentation
    "A list of classes (which subclass view). These are the views associated
with this plugin")
   (models
    :initarg :models
    :initform nil 
    :documentation
    "A list of models (which subclass model). These are the "))
  (:documentation "Represents a plugin that has been loaded."))


(defgeneric initialize-plugin (plugin)
  (:method ((plugin plugin)) nil))

;; (defgeneric load (plugin))

;; (defgeneric unload (plugin))

(defun load-plugin (plugin)
  (let ((plugin-commands (slot-value plugin 'commands)))
    (setf (elements *commands*)
          (append plugin-commands (elements *commands*))))

  (labels ((get-commands (mgroup)
             (typecase mgroup
               (command-group
                (apply #'append
                       (iter (for (name .  command) in (elements mgroup))
                         (declare (ignore name))
                         (collecting (get-commands command)))))
               (command
                (list mgroup)))))
    (let ((commands
            (apply #'append
                     (mapcar #'get-commands
                             (mapcar #'cdr (slot-value plugin 'commands))))))
      (iter (for command in commands)
        (setf (gethash (title command) *text-commands*) command)))))

