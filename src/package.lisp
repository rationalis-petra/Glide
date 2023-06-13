(defpackage glide
  (:use :cl :iterate :trivial-utf-8)
  (:export
   ;; window stuf
   :window
   :window-add-view
   ;; main function, for easy running
   :main :glide-app
   ;; Plugin hook interface
   :plugin :register-plugin
   ;; views
   :view
   :view-model :gtk-widget
   :modeline-widgets

   :text-view
   :+unicode-input-mode+
   :input-mode

   :abstract-list-view
   :make-list-item-widget
   :update-list-item-widget
   ;; models
   :model


   :text-model
   :gtk-buffer :text-model-string :text-model-insert :text-model-end-iter

   :list-model
   :emptyp
   :get-element
   :add-element))
(in-package :glide)

;; Global Variables 

