;;; core.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name
;;;
;;; Public API for iparse.

(in-package #:iparse)

;;;; Parser Record
;;;;
;;;; A parser wraps a grammar with configuration.

(defstruct (iparser (:constructor %make-iparser))
  "A compiled parser created from a grammar specification."
  (grammar nil :type hash-table :read-only t)
  (start-production nil :type keyword :read-only t))


;;;; Public Combinator Aliases

(setf (fdefinition 'string-parser) #'iparse/combinators:make-string-parser)
(setf (fdefinition 'string-ci-parser) #'iparse/combinators:make-string-ci-parser)
(setf (fdefinition 'regexp) #'iparse/combinators:make-regexp-parser)
(setf (fdefinition 'char-range) #'iparse/combinators:make-char-range-parser)
(setf (fdefinition 'cat) #'iparse/combinators:make-cat)
(setf (fdefinition 'alt) #'iparse/combinators:make-alt)
(setf (fdefinition 'ord) #'iparse/combinators:make-ord)
(setf (fdefinition 'opt) #'iparse/combinators:make-opt)
(setf (fdefinition 'plus) #'iparse/combinators:make-plus)
(setf (fdefinition 'star) #'iparse/combinators:make-star)
(setf (fdefinition 'rep) #'iparse/combinators:make-rep)
(setf (fdefinition 'nt) #'iparse/combinators:make-nt)
(setf (fdefinition 'look) #'iparse/combinators:make-look)
(setf (fdefinition 'neg) #'iparse/combinators:make-neg)

(defvar epsilon iparse/combinators:*epsilon*
  "The epsilon parser - matches empty string.")


;;;; Parser Creation

(defun parser (grammar-spec &key start)
  "Create a parser from GRAMMAR-SPEC.

GRAMMAR-SPEC can be:
  - A string containing EBNF grammar notation
  - A hash table mapping keywords to parser combinators

Options:
  :start - The start production (keyword). Defaults to first rule."
  (etypecase grammar-spec
    (string
     (multiple-value-bind (grammar start-rule)
         (iparse/cfg:build-parser grammar-spec :start start)
       (%make-iparser :grammar grammar
                      :start-production (or start start-rule))))

    (hash-table
     (%make-iparser :grammar grammar-spec
                    :start-production (or start
                                          (first (alexandria:hash-table-keys grammar-spec)))))))


;;;; Parsing Functions

(defun unwrap-result (result)
  "Recursively unwrap metaobjects from parse result, removing NIL children."
  (cond
    ((iparse/util:metaobject-p result)
     (unwrap-result (iparse/util:metaobject-value result)))
    ((iparse/afs:afs-p result)
     (remove nil (mapcar #'unwrap-result (iparse/afs:afs-to-list result))))
    ((consp result)
     (let ((children (if (consp (cdr result))
                         (remove nil (mapcar #'unwrap-result (cdr result)))
                         nil)))
       (cons (unwrap-result (car result)) children)))
    (t result)))

(defun parse (parser text &key (start nil) (partial nil) (total t))
  "Parse TEXT using PARSER.

Returns two values:
  - The parse result (parse tree on success, failure object on failure)
  - T on success, NIL on failure

Options:
  :start   - Override start production
  :partial - If T, don't require consuming all input
  :total   - If T (default), require consuming all input"
  (declare (ignore partial))
  (let* ((grammar (iparser-grammar parser))
         (start-rule (or start (iparser-start-production parser))))
    (multiple-value-bind (result success-p)
        (if total
            (iparse/gll:parse-grammar grammar start-rule text)
            (iparse/gll:parse-grammar grammar start-rule text))
      (if success-p
          (values (unwrap-result result) t)
          (values result nil)))))

(defun parses (parser text &key (start nil))
  "Parse TEXT using PARSER, returning all possible parses.

Returns a lazy list of parse results. Use CAR to get the first result,
and call (FUNCALL (CDR result)) to get the next result.

Returns NIL if no parses exist."
  (let* ((grammar (iparser-grammar parser))
         (start-rule (or start (iparser-start-production parser))))
    (iparse/gll:parse-grammar-full grammar start-rule text)))


;;;; EBNF Helper

(defun ebnf (grammar-string)
  "Parse EBNF grammar string and return a parser.
Convenience function equivalent to (parser grammar-string)."
  (parser grammar-string))


;;;; Re-export transform

(setf (fdefinition 'transform) #'iparse/transform:transform)
(setf (fdefinition 'add-line-and-column-info) #'iparse/transform:add-line-and-column-info)
