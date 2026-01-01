;;; profile.lisp
;;;
;;; Profiling for iparse

(in-package #:cl-user)

(require :sb-sprof)

(defvar *arith* (iparse:parser "
  expr = term (('+' | '-') term)*
  term = factor (('*' | '/') factor)*
  factor = number | '(' expr ')'
  number = #'[0-9]+'"))

;; Warmup
(dotimes (i 100)
  (iparse:parse *arith* "1+2*3+4*5"))

;; Profile parsing
(format t "~%=== Profiling 3000 parses ===~%~%")
(sb-sprof:with-profiling (:max-samples 5000 :report :flat :loop nil)
  (dotimes (i 3000)
    (iparse:parse *arith* "1+2*3+4*5")))
