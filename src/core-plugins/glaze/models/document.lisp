;;;; document.lisp

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

(in-package :glaze)

;; the representation of 

;;

;; (defclass node-type ()
;;   ((parser)))
(defclass glaze-model (model)
  ((root)))

(defclass node ()
  ((source)
   (params)
   (contents
    :accessor contents)))

(defgeneric render (doc type)
  (:documentation "Render a document represented by NODE to a particlar backend
  represented by the keyword TYPE (e.g. :html, :latex, ...)"))

(defmacro defnode (name &body body)
  (let* ((render (cdr (assoc :render body)))
         (type (elt (car render) 1))
         (nodesym (caar render))
         (typesym (gensym)))

    `(progn
       (defclass ,name (node) ())
       (defmethod render ((,nodesym ,name) (,typesym (eql ,type)))
         ,@(cdr render)))))

;; (defnode bold-node
;;   (:name "b")
;;   (:parse 'glaze-rule)
;;   (:render (node :html)
;;            `(:b ,(render (mapcar (render :html) (content node))))))

(defnode italic-node
  (:parse ())
  (:render (node :html)
           `(:i ,@(mapcar (alexandria:rcurry #'render :html) (contents node)))))

(defnode bold-node
  (:render (node :html)
           `(:b ,@(mapcar (alexandria:rcurry #'render :html) (contents node)))))


;; (defclass bold-node (node) ())
;; (defmethod render ((node bold-node) (type (eql :html)))
;;   `(:b ,@(mapcar (alexandria:rcurry render :html) (contents node))))

;; (defclass bold-node (node) ())
;; (defmethod render ((node bold-node) (type (eql :html)))
;;   `(:b ,@(mapcar (alexandria:rcurry render :html) (contents node))))
