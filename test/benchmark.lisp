;;; benchmark.lisp
;;;
;;; Performance benchmarks for iparse

(in-package #:cl-user)

(defvar *arith* nil)
(defvar *json* nil)

(defun setup-parsers ()
  (setf *arith* (iparse:parser "
    expr = term (('+' | '-') term)*
    term = factor (('*' | '/') factor)*
    factor = number | '(' expr ')'
    number = #'[0-9]+'"))

  (setf *json* (iparse:parser "
    value = object | array | string | number | 'true' | 'false' | 'null'
    object = <'{'> <ws> (pair (<ws> <','> <ws> pair)*)? <ws> <'}'>
    pair = string <ws> <':'> <ws> value
    array = <'['> <ws> (value (<ws> <','> <ws> value)*)? <ws> <']'>
    string = #'\"[^\"]*\"'
    number = #'-?[0-9]+(\\.[0-9]+)?'
    <ws> = #'\\s*'")))

(defun bench (name count fn)
  (let ((start (get-internal-real-time)))
    (dotimes (i count)
      (funcall fn))
    (let ((elapsed (/ (- (get-internal-real-time) start)
                      internal-time-units-per-second)))
      (format t "~A: ~D iterations in ~,3F sec (~,1F/sec)~%"
              name count elapsed (/ count elapsed)))))

(defun run-benchmarks ()
  (format t "~%Setting up parsers...~%")
  (setup-parsers)

  ;; Warmup
  (iparse:parse *arith* "1+2*3")
  (iparse:parse *json* "{\"a\":1}")

  (format t "~%=== Performance Benchmarks ===~%~%")

  ;; Arithmetic grammar
  (format t "Arithmetic Grammar:~%")
  (bench "  Simple expr '1+2*3'"      1000 (lambda () (iparse:parse *arith* "1+2*3")))
  (bench "  Nested expr '((1+2)*3)'"  500  (lambda () (iparse:parse *arith* "((1+2)*3)")))
  (bench "  Complex expr"             200  (lambda () (iparse:parse *arith* "1+2*3+4*5+6*7+8*9+10")))

  ;; JSON grammar
  (format t "~%JSON Grammar:~%")
  (bench "  Simple object"            500  (lambda () (iparse:parse *json* "{\"a\":1}")))
  (bench "  Array"                    500  (lambda () (iparse:parse *json* "[1,2,3]")))
  (bench "  Nested"                   200  (lambda () (iparse:parse *json* "{\"a\":{\"b\":[1,2,3]}}")))

  ;; Scaling test
  (format t "~%Scaling Test:~%")
  (bench "  Short input (10 chars)"     500 (lambda () (iparse:parse *json* "{\"a\":1}")))
  (bench "  Medium input (50 chars)"    200 (lambda () (iparse:parse *json* "{\"name\":\"test\",\"value\":123}")))
  (bench "  Longer input (100 chars)"   100 (lambda () (iparse:parse *json* "{\"a\":{\"b\":{\"c\":{\"d\":1}}},\"arr\":[1,2,3,4,5]}")))

  (format t "~%=== Benchmark Complete ===~%"))

(run-benchmarks)
