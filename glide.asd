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
               ;; language extensions & utility
               :alexandria
               :trivia
               :iterate
               :trivial-utf-8
               :esrap-liquid
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
               :cl-gtk4.webkit
               ;; html
               :lass
               :spinneret)
  :pathname "src"
  :components (;; glide itself
               (:file "glide" :depends-on ("defaults"))

               (:file "defaults"
                :depends-on ("core" "base" "themes"))

               ;; core components
               (:file "package" :depends-on ("extra"))

               (:module "extra"
                :pathname "extra"
                :components
                ((:module "string"
                  :pathname "string"
                  :components ((:file "package")
                               (:file "string")))
                 (:module "coll"
                  :pathname "coll"
                  :components ((:file "package")
                               (:file "coll")))))

               (:module "core"
                :pathname "core"
                :depends-on ("package" "extra")
                :components ((:file "plugin")
                             (:file "view")
                             (:file "model")
                             (:file "style")
                             (:file "keymap")
                             (:file "layout")
                             (:file "reporting")
                             (:file "commands")
                             (:file "customise")
                             (:file "minibuffer")
                             (:file "window" :depends-on ("minibuffer" "layout" "reporting" "customise"))))

               (:module "themes"
                :pathname "themes"
                :depends-on ("core")
                :components ((:file "explorer")))

               (:module "base"
                :pathname "plugins/base"
                :depends-on ("core")
                :components ((:file "package")
                             (:module "models"
                             :pathname "models"
                             :depends-on ("package")
                             :components ((:file "text")
                                          (:file "list")))
                             (:module "views"
                              :pathname "views"
                              :depends-on ("models")
                              :components ((:file "text")
                                           (:file "list")
                                           (:file "dashboard")))

                             (:file "commands" :depends-on ("package"))
                             (:file "plugin"
                              :depends-on ("views" "commands"))))


               ;; The glyph (built-in) plugin
               (:module "glyph"
                :pathname "plugins/glyph"
                :depends-on ("base")
                :components ((:file "package")
                             (:module "models"
                              :pathname "models"
                              :depends-on ("package")
                              :components ((:file "code")
                                           (:file "connection")))
                             (:module "views"
                              :pathname "views"
                              :depends-on ("models")
                              :components ((:file "connection")
                                           (:file "connections")
                                           (:file "glyph")))
                             (:file "plugin" :depends-on ("views" "models"))))

               (:module "glint"
                :pathname "plugins/glint"
                :depends-on ("base")
                :components ((:file "plugin" :depends-on ("views" "models"))
                             (:module "views"
                              :pathname "views"
                              :depends-on ("models")
                              :components ((:file "document")))
                             (:module "models"
                              :pathname "models"
                              :depends-on ("package")
                              :components ((:file "document")))
                             (:file "package")))))
