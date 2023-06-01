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
               ;; Networking (primarily intended for plugin use)
               :usocket
               :flexi-streams
               :cl-json
               ;; gui
               :cl-glib
               :cl-gtk4)
  :pathname "src"
  :components (;; glide itself
               (:file "glide" :depends-on ("core" "views"))

               ;; core components
               (:file "package")

               (:module "core"
                :pathname "core"
                :depends-on ("package")
                :components ((:file "plugin")
                             (:file "view")
                             (:file "model")
                             (:file "layout")
                             (:file "window")))

               (:module "models"
                :pathname "models"
                :depends-on ("core")
                :components ((:file "text")))

               (:module "views"
                :pathname "views"
                :depends-on ("core" "models")
                :components ((:file "text")
                             (:file "dashboard")))


               (:module "glyph"
                :pathname "glyph"
                :depends-on ("core" "views" "models")
                :components ((:file "glyph" :depends-on ("connections"))
                             (:file "connections" :depends-on ("package"))
                             (:file "package")))))
