;;; reduction.lisp
;;;
;;; SPDX-License-Identifier: EPL-1.0
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; This is a Common Lisp port of instaparse by Mark Engelberg.
;;; Original: https://github.com/Engelberg/instaparse
;;; Licensed under the Eclipse Public License 1.0.
;;;
;;; Reduction system for transforming parse results into output format.
;;; Supports hiccup format: (:tag child1 child2 ...)

(in-package #:iparse/reduction)

;;;; Reduction Types
;;;;
;;;; A reduction specifies how to transform parse results.
;;;; - :raw - Pass through without wrapping (flatten into parent)
;;;; - :hiccup - Wrap with tag: (:tag child1 child2 ...)
;;;; - function - Custom transformation function

(defstruct (reduction (:constructor %make-reduction))
  "Specifies how to transform parse results."
  (type :hiccup :type (member :raw :hiccup :custom))
  (key nil :type (or null keyword))
  (function nil :type (or null function)))

(defvar *raw-reduction*
  (%make-reduction :type :raw)
  "Reduction that passes results through without wrapping.")

(defun make-hiccup-reduction (key)
  "Create a reduction that wraps results as (:KEY child1 child2 ...)."
  (%make-reduction :type :hiccup :key key))

(defun make-custom-reduction (fn)
  "Create a reduction that applies FN to results."
  (%make-reduction :type :custom :function fn))


;;;; Applying Reductions

(defun apply-reduction (reduction result)
  "Apply REDUCTION to parse RESULT.
RESULT is typically an auto-flatten-seq from parsing."
  (ecase (reduction-type reduction)
    (:raw
     ;; Return result as-is (will be flattened into parent)
     result)

    (:hiccup
     ;; Wrap as (:key child1 child2 ...)
     (let ((key (reduction-key reduction)))
       (if (afs-p result)
           (cons key (afs-to-list result))
           (list key result))))

    (:custom
     ;; Apply custom function
     (funcall (reduction-function reduction) result))))


;;;; Standard Reductions for Grammars

(defun apply-standard-reductions (grammar)
  "Apply standard hiccup reductions to all rules in GRAMMAR.
GRAMMAR is a hash table from keywords to parser objects.
Modifies parsers in-place to add reduction specifications."
  (maphash (lambda (key parser)
             (unless (iparse/combinators:parser-reduction parser)
               (setf (iparse/combinators:parser-reduction parser)
                     (make-hiccup-reduction key))))
           grammar)
  grammar)


;;;; Singleton Detection

(defun singleton-p (parsers)
  "Return T if PARSERS is a list with exactly one element."
  (and (consp parsers)
       (null (rest parsers))))
