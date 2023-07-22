(require 'asdf)

(defsystem :glide
  :name "glide"
  :version "1.0.0"
  :maintainer "Connor Redfern"
  :author "Connor Redfern"
  :license "BSD-3"
  :description "Glide: The Glyph interactive development environment"
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
               ;;:esrap-liquid
               :closer-mop
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
                               (:file "coll")))

                 (:module "lang"
                  :pathname "lang"
                  :components ((:file "package")
                               (:file "lang")))))

               (:module "core"
                :pathname "core"
                :depends-on ("package" "extra")
                :components ((:file "plugin")
                             ;; model-view-bridge architecture
                             (:file "view")
                             (:file "model")
                             (:file "bridge" :depends-on ("model"))

                             ;; customization
                             (:file "style")
                             (:file "keymap")
                             (:file "customise")

                             ;; extension points
                             (:file "reporting")
                             (:file "commands")

                             ;;  window and layout
                             (:file "layout")
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
                                          (:file "list")
                                          (:file "settings")))
                             (:module "views"
                              :pathname "views"
                              :depends-on ("models")
                              :components ((:file "text")
                                           (:file "list")
                                           (:file "settings" :depends-on ("list"))
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
                             (:module "bridges"
                              :pathname "bridges"
                              :depends-on ("models")
                              :components ((:file "text-doc")))
                             (:module "models"
                              :pathname "models"
                              :depends-on ("package")
                              :components ((:file "document")))
                             (:file "package")))))

