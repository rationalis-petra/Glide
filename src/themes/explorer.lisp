;;;; explorer.lisp

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

(in-package :glide)

(defparameter +explorer-dark-theme+
  (make-instance 'color-theme
                 :fg-primary "#a8a7ab" ;;#c5c6c9
                 :bg-primary "#212025"

                 :fg-muted "#929096"
                 :bg-muted "#393740"

                 :fg-secondary "#83accc"
                 :bg-secondary "#1e2b40"

                 :fg-warning "#FFFF00"
                 :fg-error "#FF0000"))

(defparameter +explorer-light-theme+
  (make-instance 'color-theme
                 :fg-primary "#212025" ;;#c5c6c9
                 :bg-primary "#a8a7ab"

                 :fg-muted "#393740"
                 :bg-muted "#929096"

                 :fg-secondary "#d3d7de"
                 :bg-secondary "#304057"

                 :fg-warning "#FFFF00"
                 :fg-error "#FF0000"))
