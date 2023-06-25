(require 'asdf)

(defsystem :glide
  :name "glide"
  :version "1.0.0"
  :maintainer "Connor Redfern"
  :author "Connor Redfern"
  :license "BSD-3"
  :description "An editor for Glyph source code"
  :defsystem-depends-on (:deploy)
  :build-operation "deploy-op"
  :build-pathname "glide"
  :entry-point "glide:main"
  :depends-on (;; system
               :asdf
               ;; language extensions
               :alexandria
               :trivia
               :iterate
               :trivial-utf-8
               ;; portability
               :uiop
               :bordeaux-threads
               ;; Networking (primarily intended for plugin use)
               :usocket
               :flexi-streams
               :yason
               ;; gui
               :cl-glib
               :cl-gdk4
               :cl-gtk4
               :lass)
  :pathname "src"
  :components (;; glide itself
               (:file "glide" :depends-on ("defaults"))

               (:file "defaults"
                :depends-on ("core" "views" "models" "themes"))

               ;; core components
               (:file "package")

               (:module "core"
                :pathname "core"
                :depends-on ("package")
                :components ((:file "plugin")
                             (:file "view")
                             (:file "model")
                             (:file "style")
                             (:file "layout")
                             (:file "reporting")
                             (:file "customise")
                             (:file "window" :depends-on ("layout" "reporting" "customise"))))

               (:module "models"
                :pathname "models"
                :depends-on ("core")
                :components ((:file "text")
                             (:file "list")))

               (:module "views"
                :pathname "views"
                :depends-on ("core" "models")
                :components ((:file "text")
                             (:file "list")
                             (:file "dashboard")))

               (:module "themes"
                :pathname "themes"
                :depends-on ("core")
                :components ((:file "explorer")))

               ;; The glyph (built-in) plugin
               (:module "glyph"
                :pathname "glyph"
                :depends-on ("core" "views" "models")
                :components ((:file "plugin" :depends-on ("views" "models"))
                             (:module "views"
                              :pathname "views"
                              :depends-on ("models")
                              :components ((:file "connection")
                                           (:file "connections")
                                           (:file "glyph")))
                             (:module "models"
                              :pathname "models"
                              :depends-on ("package")
                              :components ((:file "code")
                                           (:file "connection")))
                             (:file "package")))))
