(in-package :glide)
;; Example 1: a simple image viewer plugin 
;; (defplugin :image-viwer
;;   :views (image-view image-editor)
;;   :models (image)
;;   :init load-imagemagik)

;; Globals relatedto plugins
(defvar *plugins* (make-hash-table))
(defvar *initializers* nil)
(defvar *menu-bar* nil)
(defvar *commands* nil)
(defvar *action-map* nil)   ;; 
(defvar *start-view* nil)   ;; 
(defvar *registered-plugins* nil)

(defun register-plugin (plugin))

(defun register-models (plugin models))

(defun register-views (plugin views)
"For PLUGIN, register the list of VIEWS. This means two things:
1. The list VIEWS is associated with the plugin.
2. Glide will look at the models associated with each VIEW, and make sure
   they are available for that model. Further, a given view may request to be
   the default view for a given datatype.")
 

(defmacro defplugin (plugin-name &key (init '(lambda())) views models)
  `(setf (gethash plugin-name *plugins*)
         (lambda (app)
           (,init app)
           ,(when models `(register-models ,plugin-name
                           (list ,@(mapcar
                                    (lambda (m) `(find-class ,m))
                                    models))))
           ,(when views `(register-views ,plugin-name
                          (list ,@(mapcar
                                   (lambda (v) `(find-class ,v))
                                   views)))))))


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
    :documentation
    "A List of submenus that are always present in the menu bar when this plugin
  is loaded.")
   (commands
    :initarg :commands
    :documentation
    "A list of (named) commands that are always present when this plugin is loaded")
   (views
    :initarg :views
    :documentation
    "A list of classes (which subclass view). These are the views associated
with this plugin")
   (models
    :initarg :models
    :documentation
    "A list of models (which subclass model). These are the "))
  (:documentation "Represents a plugin that has been loaded."))


;; (defgeneric initialize (plugin))

;; (defgeneric load (plugin))

;; (defgeneric unload (plugin))
