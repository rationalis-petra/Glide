;;;; document.lisp
rse
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

(in-package :glint)


(defclass glint-model (model)
  ((root
    :accessor root
    :initarg :root
    :type node)))

(defclass node ()
  ((source)
   (params)
   ;; (contents
   ;;  :initarg :contents
   ;;  :accessor contents)
   ))

(defgeneric render (doc type)
  (:documentation "Render a document represented by NODE to a particlar backend
  represented by the keyword TYPE (e.g. :html, :latex, ...)"))

;; Rendering
(defmethod render ((node glint-model) (type (eql :html)))
  (let ((html-out (make-string-output-stream)))
    (spinneret:interpret-html-tree
     `(:html
       (:body
        ,(render (root (make-default-doc)) :html)))
     :stream html-out)
    (get-output-stream-string html-out)))

(defmethod render ((node string) (type (eql :html))) node)

;; Parsing
(defvar *node-types* (make-hash-table))

(define-esrap-env gdn)

(define-gdn-rule node ()
  #\⦗
  (pred (lambda (c) (not (char= #\|))) character)
  #\|
  (pred (lambda (c) (not (char= #\⦘))) character)
  #\⦘)

(defun parse-default (string) string)

(define-esrap-env glint)

(define-glint-rule any () (|| bold))

(define-glint-rule bold ()
    (pred (lambda (c) (not (char= #\*))) character))

;; Parser of Glint Documents:
;; Glint documents are in a pseudo-markdown format
;; They are intended specifically for STEM note-taking. As such, there are
;; specific facilities for
;; + Naming things like definitions and theorems, and linking them to examples, 
;;   proofs, etc. 
;; + Bibliographies/referencing
;; + Mathematical notation (Via TeX)
;; + Constructing and rendering domain-specific diagrams via Glyph, e.g.
;;   + Proof Tress
;;   + Category-theory diagrams
;;   + Graphs (graph-theory)
;;   + Molecules
;;   + Plots
;; + Embedding data/objects and mathematical/statistical analysis.
;; + ...
;; Essentially a very powerful, functional python notebook.
;; 
;; Text formatting: like markdown:
;; + ** → bold
;; + __ → italic
;; + ~~ → strikethrough
;; + `` → monospace
;; + \  → escape
;; + #ⁿ → titles
;; Caveats
;; + Text under titles/sections must be indented!
;; + 
;; 
;; Complex objects: documents+node structure 
;; + [table| --table description ]
;; + [M| -- latex math]
;; + [m| -- latex math (inline)]
;; + [m| -- latex math (inline)]
;; + [plist| property list]
;; + [def, for| definition text]
;; + [ex, for| example context]
;; + [ex, for| example context]


(defun parse-document (string) string)

(defmacro defnode (name &body body)
  (let* ((render (cdr (assoc :render body)))
         (type (elt (car render) 1))
         (nodesym (caar render))
         (typesym (gensym))

         (parse (cdr (assoc :parse body)))
         (parse-name     (elt parse 0))
         (parse-function (elt parse 1)))

    `(progn
       (defclass ,name (node) ())
       (setf (gethash ,parse-name *node-types*) ,parse-function)
       (defmethod render ((,nodesym ,name) (,typesym (eql ,type)))
         ,@(cdr render)))))


(defnode italic-node
  (:parse "i" #'(lambda (x) x))
  ;(:child string)
  (:render
   (node :html) `(:i ,(contents node))))

(defnode bold-node
  (:parse "b" #'(lambda (x) x))
  ;(:child string)
  (:render
   (node :html)
   `(:b ,(contents node))))

(defnode paragraph-node
  (:parse "p" #'parse-default)
  (:render
   (node :html)
   `(:p ,@(mapcar (⟜ #'render :html) (contents node)))))

(defnode text-node
  (:parse "raw" (lambda (x) x))
  (:render
   (node :html) (contents node)))


(defun make-default-doc ()
    (make-instance 'glint-model
     :root
     (make-instance
      'paragraph-node
      :contents (list
                 "Hello, World! "
                 (make-instance 'bold-node :contents "Hello")
                 (make-instance 'italic-node :contents "World")))))

