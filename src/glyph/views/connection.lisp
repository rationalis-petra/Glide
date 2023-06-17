;;;; connection.lisp

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

(defclass connection-view (view) ())

(defmethod initialize-instance :after ((view connection-view) &key model)
  (let* ((box (gtk4:make-box :orientation gtk4:+orientation-vertical+
                             :spacing 0))
         (repl-view (gtk4:make-text-view
                     :buffer (glide:gtk-buffer (repl-model model)))))

    ;; Set style
    (let ((style (gtk4:make-css-provider)))
      (gtk4:css-provider-load-from-data style
                                        "textview { font-family: JuliaMono; font-size: 10pt; }")
      (gtk4:style-context-add-provider (gtk4:widget-style-context repl-view) style glib:+maxuint32+))

    (gtk4:box-append box repl-view)
    (setf (gtk-widget view) box)))

