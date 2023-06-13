(in-package :glide)

(defclass list-model (model)
  ((lisp-vals
    :accessor lisp-vals
    :initform (make-hash-table))
   (gtk-list
    :accessor gtk-list
    ;; :initform (gio:make-list-store :item-type (gobject:type-from-name "GValue"))
    :initform (gtk4:make-string-list :strings ())))
  (:documentation "Wraps a GTK4 list model. Elements are assumed to be CL values"))

(defun emptyp (model)
  (equal (hash-table-count (lisp-vals model)) 0))

(defun get-element (n model)
  ;; TODO: this function is dodgy as shit
  (declare (ignore n))
  (iter (for (key val) in-hashtable (lisp-vals model)) (return val)))

(defgeneric add-element (element model)
  (:documentation "Add ELEMENT into MODEL. ELEMENT is placed at the head of the list")
  (:method (element (model list-model))
    (let ((name (string (gensym))))
      (setf (gethash name (lisp-vals model)) element)
      (gio:list-store-append (gtk-list model) name))))

(defgeneric pop-element (model)
  (:documentation "Remove the first element from MODEL"))

(defgeneric remove-element-if (predicate model)
  (:documentation "Remove all elements for which PREDICATE does not return nil"))

(defgeneric remove-element (element model &key test)
  (:documentation "Remove all elements e from MODEL for which (test ELEMENT e)
returns a generalized truth value"))
