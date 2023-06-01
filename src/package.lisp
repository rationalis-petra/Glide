(defpackage glide
  (:use :cl :iterate :trivial-utf-8)
  (:export
   ;; window stuf
   :window
   :window-add-view
   ;; main function, for easy running
   :main
   ;; Plugin hook interface
   :plugin :register-plugin
   ;; views
   :view :text-view
   ;; models
   :model :text-model))
(in-package :glide)

;; Global Variables 

