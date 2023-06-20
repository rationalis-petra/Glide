;;;; glyph.lisp

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

(defclass glyph-view (text-view) ())

(defmethod initialize-instance :after ((view glyph-view) &key model)
  (let ((eval-btn (gtk4:make-button :label "Eval"))
        (load-btn (gtk4:make-button :label "Load")))
    (gtk4:connect eval-btn "clicked"
                  (lambda (btn)
                    (declare (ignore btn))
                    (unless (emptyp *gl-connections-model*)
                      (eval-code
                       (get-element 0 *gl-connections-model*)
                       (text-model-string (view-model view))))))
    (gtk4:connect load-btn "clicked"
                  (lambda (btn)
                    (declare (ignore btn))
                    (unless (emptyp *gl-connections-model*)
                      (load-code
                       (get-element 0 *gl-connections-model*)
                       (text-model-string (view-model view))))))

    (with-slots (modeline-widgets) view
      (push load-btn modeline-widgets)
      (push eval-btn modeline-widgets))))
