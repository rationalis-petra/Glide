;;;; string.lisp

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


;; utilities for manipulating strings

(in-package :extra.string)

(defun split-on (string char &key (empty-substrs :keep))
  "Split STRING into substrings with instances of CHAR marking the boundary
between strings. Behaviour can be modified by ADJACENT-SPLITS"
  (let ((indices (iter (for i from 0 to (length string))
                   (when (or (= i (length string))
                             (char= (elt string i) char))
                     (collect i)))))

    (ecase empty-substrs
      (:remove
       (iter (for i in indices)
             (for j previous i initially -1)
         (unless (= (+ 1 j) i)
           (collect (subseq string (+ 1 j) i)))))
      (:keep
       (iter (for i in indices)
             (for j previous i initially -1)
         (collect (subseq string (+ 1 j) i)))))))





