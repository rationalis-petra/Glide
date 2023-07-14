;;;; coll.lisp

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

(in-package :extra.coll)

(defun ¨ (func &rest seqs) (map 'vector func seqs))

(defun ⌿ (func &rest args) (apply #'reduce (cons func args)))

(defun ⍀ (func seq)
  (let ((state nil)
        (fst nil))
    (flet ((scan (arg)
             (if fst
                 (let ((result (funcall func state arg)))
                   (setf state result)
                   result)
                 (progn (setf fst t)
                        (setf state arg)
                        arg))))
      (map 'vector #'scan seq))))

(defun ⟜ (func &rest args) (apply #'alexandria:rcurry (cons func args)))

(defun ⊸ (func &rest args) (apply #'alexandria:curry (cons func args)))

(defun ∘ (func &rest more-functions)
  (apply #'alexandria:compose (cons func more-functions)))
