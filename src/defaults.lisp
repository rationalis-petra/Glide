;;;; defaults.lisp

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

;; Default values for various settings/customisation options


(setf
 *default-theme* +explorer-theme+

 *make-default-layout*
 (lambda (parent)
   (make-instance 'single-layout
                  :child (make-instance 'dashboard-view)))

 *make-default-view* 
 (lambda () (make-instance 'dashboard-view)))
