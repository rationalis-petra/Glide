;;;; connections.lisp

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

(defclass connections-list-view (abstract-list-view) ())

(defclass connections-view (view) ())


(defmethod make-list-item-widget ((view connections-list-view))
  (gtk4:make-label :str "Connection"))

(defmethod update-list-item-widget ((view connections-list-view) connection widget))

(defmethod initialize-instance :after ((view connections-view) &key model)
  (let* ((widget (gtk4:make-box
                  :orientation gtk4:+orientation-vertical+
                  :spacing 0))
         (list-view (make-instance 'connections-list-view :model model))
         (connections model)

         (add-connection-row (gtk4:make-box
                              :orientation gtk4:+orientation-horizontal+
                              :spacing 0))
         (hostname-text-box (gtk4:make-entry))
         (port-text-box (gtk4:make-entry))

         (add-connection-btn (gtk4:make-button :label "New"))
         (clear-connection-btn (gtk4:make-button :label "Clear")))

    (flet ((new-connection (button)
             (declare (ignore button))
             (let* ((host-text (gtk4:entry-buffer-text (gtk4:entry-buffer
                                                        hostname-text-box)))
                    (port-text (gtk4:entry-buffer-text (gtk4:entry-buffer port-text-box))))
             (make-connection
              :host host-text
              :port (parse-integer port-text))))
           (clear-connections (button)
             (declare (ignore button))
             (clear *gl-connections-model*)))
      (gtk4:connect add-connection-btn "clicked" #'new-connection)
      (gtk4:connect clear-connection-btn "clicked" #'clear-connections))

    (gir:invoke ((gtk4:text-buffer hostname-text-box) 'set-text)
                (string "localhost") (length "localhost"))
    (gir:invoke ((gtk4:text-buffer port-text-box) 'set-text)
                (string "8801") (length "8801"))

    (gtk4:box-append add-connection-row hostname-text-box)
    (gtk4:box-append add-connection-row port-text-box)
    (gtk4:box-append add-connection-row add-connection-btn)
    (gtk4:box-append add-connection-row clear-connection-btn)

    (gtk4:box-append widget (gtk-widget list-view))
    (gtk4:box-append widget add-connection-row)

    (setf (gtk-widget view) widget)))
