;;; packages.lisp
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name
;;;
;;; Package definitions for iparse.

(defpackage #:iparse/util
  (:use #:cl)
  (:export
   ;; Segment (O(1) substring)
   #:segment
   #:make-segment
   #:segment-p
   #:segment-string
   #:segment-offset
   #:segment-length
   #:segment-char
   #:segment-subseq
   #:segment-to-string
   #:segment-remaining

   ;; Utilities
   #:make-adjustable-vector
   #:with-meta
   #:get-meta
   #:unwrap-meta
   #:metaobject
   #:metaobject-p
   #:metaobject-value
   #:metaobject-metadata
   #:make-keyword))

(defpackage #:iparse/afs
  (:use #:cl #:iparse/util)
  (:export
   #:auto-flatten-seq
   #:afs-p
   #:afs-count
   #:afs-empty-p
   #:afs-dirty-p
   #:*afs-empty*
   #:make-afs
   #:conj-flat
   #:afs-to-list
   #:afs-to-vector))

(defpackage #:iparse/combinators
  (:use #:cl #:iparse/util)
  (:export
   ;; Base class
   #:parser
   #:parser-hide
   #:parser-reduction

   ;; Terminal parsers
   #:string-parser
   #:string-parser-p
   #:parser-string

   #:string-ci-parser
   #:string-ci-parser-p

   #:regexp-parser
   #:regexp-parser-p
   #:parser-pattern
   #:parser-scanner

   #:char-range-parser
   #:char-range-parser-p
   #:parser-lo
   #:parser-hi

   ;; Composition parsers
   #:cat-parser
   #:cat-parser-p
   #:parser-parsers

   #:alt-parser
   #:alt-parser-p

   #:ord-parser
   #:ord-parser-p
   #:parser-parser1
   #:parser-parser2

   ;; Quantifier parsers
   #:opt-parser
   #:opt-parser-p
   #:parser-parser

   #:plus-parser
   #:plus-parser-p

   #:star-parser
   #:star-parser-p

   #:rep-parser
   #:rep-parser-p
   #:parser-min
   #:parser-max

   ;; Reference and lookahead
   #:nt-parser
   #:nt-parser-p
   #:parser-keyword

   #:look-parser
   #:look-parser-p

   #:neg-parser
   #:neg-parser-p

   #:epsilon-parser
   #:epsilon-parser-p
   #:*epsilon*

   ;; Constructors
   #:make-string-parser
   #:make-string-ci-parser
   #:make-regexp-parser
   #:make-char-range-parser
   #:make-cat
   #:make-alt
   #:make-ord
   #:make-opt
   #:make-plus
   #:make-star
   #:make-rep
   #:make-nt
   #:make-look
   #:make-neg
   #:hide
   #:hide-tag

   ;; Utilities
   #:copy-parser))

(defpackage #:iparse/reduction
  (:use #:cl #:iparse/util #:iparse/afs)
  (:export
   #:reduction
   #:reduction-p
   #:reduction-type
   #:reduction-key
   #:make-hiccup-reduction
   #:*raw-reduction*
   #:apply-reduction
   #:apply-standard-reductions
   #:singleton-p))

(defpackage #:iparse/failure
  (:use #:cl #:iparse/util)
  (:export
   ;; Failure structure
   #:parse-failure
   #:parse-failure-p
   #:parse-failure-index
   #:parse-failure-reasons
   #:parse-failure-line
   #:parse-failure-column
   #:parse-failure-text-line
   #:parse-failure-input
   #:make-failure
   #:merge-failures
   #:augment-failure
   #:format-failure
   ;; Condition
   #:iparse-error
   #:iparse-error-failure
   #:signal-parse-error))

(defpackage #:iparse/gll
  (:use #:cl #:iparse/util #:iparse/afs #:iparse/combinators
        #:iparse/reduction #:iparse/failure)
  (:export
   #:parse-grammar
   #:parse-grammar-full))

(defpackage #:iparse/cfg
  (:use #:cl #:iparse/util #:iparse/afs #:iparse/combinators
        #:iparse/reduction #:iparse/gll)
  (:export
   #:*cfg*
   #:build-parser
   #:ebnf
   ;; Internal but used by abnf
   #:tree-tag
   #:tree-contents
   #:tree-content
   #:unwrap-tree
   #:process-regexp
   #:collect-non-terminals
   #:check-grammar))

(defpackage #:iparse/abnf
  (:use #:cl #:iparse/util #:iparse/afs #:iparse/combinators
        #:iparse/reduction #:iparse/gll)
  (:export
   #:*abnf*
   #:*abnf-core*
   #:build-abnf-parser
   #:abnf))

(defpackage #:iparse/transform
  (:use #:cl)
  (:export
   #:transform
   #:add-line-and-column-info))

(defpackage #:iparse
  (:use #:cl)
  (:import-from #:iparse/combinators
                #:make-string-parser
                #:make-string-ci-parser
                #:make-regexp-parser
                #:make-char-range-parser
                #:make-cat
                #:make-alt
                #:make-ord
                #:make-opt
                #:make-plus
                #:make-star
                #:make-rep
                #:make-nt
                #:make-look
                #:make-neg
                #:hide
                #:hide-tag
                #:*epsilon*)
  (:import-from #:iparse/failure
                #:parse-failure
                #:parse-failure-p
                #:iparse-error)
  (:import-from #:iparse/transform
                #:transform
                #:add-line-and-column-info)
  (:export
   ;; Core API
   #:parser
   #:defparser
   #:parse
   #:parses
   #:with-parser

   ;; Special variables
   #:*signal-errors*
   #:*parse-partial*
   #:*print-parse-tree-indent*

   ;; Conditions and restarts
   #:iparse-error
   #:parse-failure
   #:parse-failure-p

   ;; Combinators (for programmatic grammar building)
   #:string-parser
   #:string-ci-parser
   #:regexp
   #:char-range
   #:cat
   #:alt
   #:ord
   #:opt
   #:plus
   #:star
   #:rep
   #:nt
   #:look
   #:neg
   #:epsilon
   #:hide
   #:hide-tag

   ;; EBNF helper
   #:ebnf

   ;; Pretty printing
   #:pprint-parse-tree
   #:parse-tree-to-string

   ;; Generic functions for extensibility
   #:parse-node
   #:transform-with-methods

   ;; Post-processing
   #:transform
   #:add-line-and-column-info))
