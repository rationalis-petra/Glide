;;;; plugin.lisp

;; Copyright (C) 2023 Connor Redfern
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(in-package :glyph)

(defun hash-table-from-list (list)
  (iterate (for (key val) in list)
    (with table = (make-hash-table))
    (setf (gethash key table) val)
    (finally (return table))))


(defun list-connections (window)
  (window-add-view
   window
   (make-instance 'connections-view :model *gl-connections-model*)))


(defun new-playground (window)
  (window-add-view
   window
   (make-instance 'glyph-view
                  :model (make-instance 'text-model)
                  :input-mode +unicode-input-mode+)))


(defun show-server (window)
  (if (not (emptyp *gl-connections-model*))
      (window-add-view
       window
       (make-instance 'connection-view
                      :model (get-element 0 *gl-connections-model*))
       :location-preference :window-bottom)
      (message-error "No server available")))

(defparameter +glyph-plugin+
  (make-instance
   'plugin
   :name "Glyph"
   :about "A plugin for the Glyph Language"
   :commands (hash-table-from-list
              (list
               (list "glyph:connections" #'list-connections)
               (list "glyph:server" #'show-server)
               (list "glyph:playground" #'new-playground)))

   :views (list 'glyph-view)

   :menu-bar-submenus (list
                       (list "Views"
                             (cons "Connections" #'list-connections)
                             (cons "Server" #'show-server)
                             (cons "Playground" #'new-playground)))
   :models ()))

;; (defvar make-glyph-playground-layout ()
;;   (make-vertical-layout 
;;     ()))

(defvar has-init nil)
(unless has-init
  (progn
    ;; TODO: remove this!
    (setf has-init t)
    ;; (make-connection)
    ;; (setf *make-start-layout* #'make-glyph-playground-layout)
    (register-plugin +glyph-plugin+)))
