(require 'asdf)

(defsystem :glide
  :name "glide"
  :version "1.0.0"
  :maintainer "Connor Redfern"
  :author "Connor Redfern"
  :license "BSD-3"
  :description "An editor for Glyph source code"
  :depends-on (;; language extensions
               :trivial-utf-8
               :alexandria
               :iterate
               :asdf
               :bordeaux-threads
               :cl-glib
               :cl-gtk4)
  :pathname "src/"
  :components ((:file "glide" :depends-on ("window" "package"))
               (:file "window" :depends-on ("package"))
               (:file "package")))
