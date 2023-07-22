;;;; doc-text.lisp

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

(defclass text↦doc (bridge) ())


(defmethod bridge-update ((bridge text↦doc))
  (let ((text (text-model-string (source bridge))))
    (match (parse-glide-markup (make-string-input-stream text))
      ((cons :ok val)
       (setf (root (destination bridge)) val))
      ((cons :err err)
       (message-error "malformed markup")))))


(defmethod bridge-delta-update ((bridge text↦doc) delta))


(define-condition parser-error (error)
  ((message :initarg :message :accessor message)))


;; Parsing Helper Functions
(defun parse-till-peek (stream func)
  (let ((out (make-string-output-stream)))
    (iter (for char = (peek-char nil stream nil)) 
      (if (and char (not (funcall func char)))
          (write-char (read-char stream) out)
          (finish)))
      
    (get-output-stream-string out)))

(defun parse-till (stream func)
  (let ((out (make-string-output-stream)))
    (iter (for char = (read-char stream nil)) 
      (if (and char (not (funcall func char)))
          (write-char char out)
          (finish)))
      
    (get-output-stream-string out)))

(defun parse-while (stream func) (parse-till (∘ #'not func)))


;; classical recursive-descent parser
(defun parse-glide-markup (stream)
  (labels
      ((markup (s)
         (make-instance
          'document-node
          :contents
          (iter (for char = (peek-char nil s nil))
            (cond
              ((null char) (finish))
              ((eq char #\Newline) (read-char s))
              (t (collect (paragraph s)))))))

       (paragraph (s)
         (make-instance
          'paragraph-node
          :contents
          (iter (for char = (read-char s nil))
            ;; if end paragraph
            (if (and char
                     (not (and (eq char #\Newline)
                               (eq (peek-char nil s nil) #\Newline))))

              (collect
                  (case char
                    (#\* (bold s) )
                    (#\_ (italic s))
                    (#\~ (strikethrough s))
                    (#\` (inlmono s))
                    ;; (#\$ (blockmono ))
                    ;; end of stream 
                    (t (text s char))))
              (finish)))))



       (text (s char)
         (let ((str (parse-till-peek s (⟜ #'find "*_`~
"))))
           (make-instance 'text-node
                          :contents (concatenate 'string (string char) str))))
       (bold (s)
         (let ((str (parse-till s (⟜ #'char= #\*))))
           (make-instance 'bold-node :contents str)))
       (italic (s)
         (let ((str (parse-till s (⟜ #'char= #\_))))
           (make-instance 'italic-node :contents str)))
       (strikethrough (s)
         (let ((str (parse-till s (⟜ #'char= #\~))))
           (make-instance 'bold-node :contents str)))
       (inlmono (s)
         (let ((str (parse-till s (⟜ #'char= #\`))))
           (make-instance 'mono-node :contents str)))
       ;; (blockcode (s) ())
       )

    ;; TODO: handle EOF errors
    (handler-case (cons :ok (markup stream))
      (parser-error (err) (cons :err (message err))))))


