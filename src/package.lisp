(defpackage glide
  (:use :cl :iterate :trivial-utf-8)
  (:export
   ;; main function, for easy running
   :main
   ;; Plugin hook interface
   :defplugin :plugin
   ;; views
   :view :text-view
   ;; models
   :model :text-model

   ;; DEPRICATED
   ;; globals, for use by packages
   :*initializers* :*commands* :*menu-bar* :*action-map* :*start-view*))
(in-package :glide)

;; Global Variables 

(defvar *plugins* (make-hash-table))
(defvar *initializers* nil) ;; remove me!
(defvar *menu-bar* nil)     ;; remove me!
(defvar *commands* nil)
(defvar *action-map* nil)   ;; 
(defvar *start-view* nil)   ;; 
