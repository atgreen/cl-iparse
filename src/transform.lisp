;;; transform.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name
;;;
;;; Post-parse tree transformation utilities.

(in-package #:iparse/transform)

;;;; Transform - Apply Transformations to Parse Tree

(defun transform (transform-map parse-tree)
  "Apply transformations from TRANSFORM-MAP to PARSE-TREE.
TRANSFORM-MAP is a hash table from keywords to transformation functions.
Each function receives the children of a matching node as arguments."
  (labels ((transform-node (node)
             (cond
               ;; String leaf: return as-is
               ((stringp node)
                node)

               ;; Hiccup format: (:tag child1 child2 ...)
               ((and (consp node) (keywordp (first node)))
                (let* ((tag (first node))
                       (children (rest node))
                       (transformed-children (mapcar #'transform-node children))
                       (transform-fn (gethash tag transform-map)))
                  (if transform-fn
                      (apply transform-fn transformed-children)
                      (cons tag transformed-children))))

               ;; List (possibly hidden root): transform each element
               ((listp node)
                (mapcar #'transform-node node))

               ;; Other atoms: return as-is
               (t node))))
    (transform-node parse-tree)))


;;;; Line and Column Information

(defun add-line-and-column-info (text parse-tree)
  "Add :line, :column, :end-line, :end-column metadata to parse tree nodes.
Uses metadata attached during parsing if available."
  (let ((line-starts (compute-line-starts text)))
    (labels ((index->line-col (index)
               (let ((line (position-if (lambda (start) (> start index)) line-starts)))
                 (if line
                     (let* ((line-num line)
                            (line-start (aref line-starts (1- line-num)))
                            (col (1+ (- index line-start))))
                       (values line-num col))
                     (let* ((line-num (length line-starts))
                            (line-start (aref line-starts (1- line-num)))
                            (col (1+ (- index line-start))))
                       (values line-num col)))))

             (annotate (node)
               (cond
                 ((stringp node) node)

                 ((and (consp node) (keywordp (first node)))
                  (let* ((tag (first node))
                         (children (rest node))
                         (meta (iparse/util:get-meta node))
                         (start-index (getf meta :start-index))
                         (end-index (getf meta :end-index)))
                    (if (and start-index end-index)
                        (multiple-value-bind (start-line start-col)
                            (index->line-col start-index)
                          (multiple-value-bind (end-line end-col)
                              (index->line-col end-index)
                            (list* tag
                                   :line start-line
                                   :column start-col
                                   :end-line end-line
                                   :end-column end-col
                                   (mapcar #'annotate children))))
                        (cons tag (mapcar #'annotate children)))))

                 ((listp node)
                  (mapcar #'annotate node))

                 (t node))))

      (annotate parse-tree))))

(defun compute-line-starts (text)
  "Compute a vector of character indices where each line starts."
  (let ((starts (make-array 1 :adjustable t :fill-pointer 0)))
    (vector-push-extend 0 starts)
    (loop for i from 0 below (length text)
          when (char= (char text i) #\Newline)
            do (vector-push-extend (1+ i) starts))
    starts))
