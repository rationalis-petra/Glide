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

(in-package :glint)


(defclass glint-model (model)
  ((root
    :accessor root
    :initarg :root
    :initform (make-instance 'document-node :contents nil)
    :type node)))


(defclass node ()
  ((source)
   (params)))

(defmethod (setf root) :after (root (model glint-model))
  (update-notify model))

(defgeneric render (doc type)
  (:documentation "Render a document represented by NODE to a particlar backend
  represented by the keyword TYPE (e.g. :html, :latex, ...)"))

(defun render-html-body (doc &key style)
  (let ((html-out (make-string-output-stream)))
    (spinneret:interpret-html-tree
     `(:html
       (:script :src "https://polyfill.io/v3/polyfill.min.js?features=es6")
       (:script :id "MathJax-script"
                :|async src| "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")
       (:script (:raw "
MathJax = {
  tex: {
    inlineMath: [              // start/end delimiter pairs for in-line math
      ['$', '$']
    ],
    displayMath: [             // start/end delimiter pairs for display math
      ['$$', '$$'],
    ],
  }
};
"))
       (:style (:raw ,style))
       ,doc)
     :stream html-out)
    (get-output-stream-string html-out)))

;; Rendering
(defmethod render ((model glint-model) (type (eql :html)))
  (render (root model) type))


(defmethod render ((node string) (type (eql :html))) node)

;; Parsing
(defvar *node-types* (make-hash-table))


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
         (typesym (gensym "type"))

         (contents-type (cadr (assoc :contents body)))

         (parse (cdr (assoc :parse body)))
         (parse-name     (elt parse 0))
         (parse-function (elt parse 1)))

    `(progn
       (defclass ,name (node)
         ((contents
           :initarg :contents
           :accessor contents
           :type ,contents-type)))
       (setf (gethash ,parse-name *node-types*) ,parse-function)
       (defmethod render ((,nodesym ,name) (,typesym (eql ,type)))
         ,@(cdr render)))))


(defnode italic-node
  (:parse "i" #'(lambda (x) x))
  (:contents string)
  (:render
   (node :html) `(:i ,(contents node))))

(defnode bold-node
  (:parse "b" #'(lambda (x) x))
  (:contents string)
  (:render
   (node :html)
   `(:b ,(contents node))))

(defnode mono-node
  (:parse "mono" #'(lambda (x) x))
  (:contents string)
  (:render
   (node :html)
   `(:code ,(contents node))))

(defnode paragraph-node
  (:parse "p" #'(lambda (x) x)) ;; parse-default
  (:contents list)
  (:render
   (node :html)
   `(:p ,@(mapcar (⟜ #'render :html) (contents node)))))

(defnode text-node
  (:parse "raw" (lambda (x) x))
  (:contents string)
  (:render
   (node :html) (contents node)))

(defnode document-node
  (:parse "doc" #'(lambda (x) x)) ;; parse-default
  (:contents list)
  (:render
   (node :html)
   `(:body ,@(mapcar (⟜ #'render :html) (contents node)))))
