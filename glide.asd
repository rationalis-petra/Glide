(require 'asdf)

(defsystem :glide
  :name "glide"
  :version "1.0.0"
  :maintainer "Connor Redfern"
  :author "Connor Redfern"
  :license "BSD-3"
  :description "An editor for Glyph source code"
  :depends-on (;; system
               :asdf
               ;; language extensions
               :alexandria
               :iterate
               :trivial-utf-8
               ;; portability
               :bordeaux-threads
               ;; networking 
               :usocket
               :flexi-streams
               :cl-json
               ;; gui
               :cl-glib
               :cl-gtk4)
  :pathname "src/"
  :components (;; Inbuild modules
               (:file "glyph" :depends-on ("glide"))

               ;; glide itself
               (:file "glide" :depends-on ("window"))

               ;; core components
               (:file "window" :depends-on ("package"))
               (:file "package")))
