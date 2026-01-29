#!/usr/bin/env sbcl --script
;;;
;;; Quick launcher for DOT parser example
;;; Usage: sbcl --script test/run-dot-example.lisp
;;;

(require :asdf)
(asdf:load-system :iparse)
(load "test/example-dot.lisp")

;; Run all examples
(iparse/examples/dot:run-dot-examples)

;; Run tests
(iparse/examples/dot:test-dot-parser)
