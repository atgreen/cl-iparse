;;; tests.lisp
;;;
;;; Test suite for iparse

(defpackage #:iparse/tests
  (:use #:cl #:iparse)
  (:import-from #:iparse/abnf #:build-abnf-parser)
  (:export #:run-all-tests))

(in-package #:iparse/tests)

;;;; Test Infrastructure

(defvar *test-count* 0)
(defvar *pass-count* 0)
(defvar *fail-count* 0)

(defmacro deftest (name &body body)
  `(defun ,name ()
     (format t "~&  ~A... " ',name)
     (incf *test-count*)
     (handler-case
         (progn ,@body
                (incf *pass-count*)
                (format t "PASS~%"))
       (error (e)
         (incf *fail-count*)
         (format t "FAIL~%    ~A~%" e)))))

(defmacro assert-equal (expected actual)
  `(let ((exp ,expected)
         (act ,actual))
     (unless (equal exp act)
       (error "Expected ~S, got ~S" exp act))))

(defmacro assert-true (form)
  `(unless ,form
     (error "Expected true: ~S" ',form)))

(defmacro assert-parse-success (parser input)
  `(multiple-value-bind (result success-p)
       (parse ,parser ,input)
     (unless success-p
       (error "Parse failed: ~A" result))))

(defmacro assert-parse-result (parser input expected)
  `(multiple-value-bind (result success-p)
       (parse ,parser ,input)
     (unless success-p
       (error "Parse failed: ~A" result))
     (unless (equal result ,expected)
       (error "Expected ~S, got ~S" ,expected result))))


;;;; Basic Combinator Tests

(deftest test-string-parser
  (let ((p (parser "s = 'hello'")))
    (assert-parse-result p "hello" '(:S "hello"))))

(deftest test-regexp-parser
  (let ((p (parser "num = #'[0-9]+'")))
    (assert-parse-result p "123" '(:NUM "123"))))

(deftest test-alternation
  (let ((p (parser "choice = 'a' | 'b' | 'c'")))
    (assert-parse-result p "a" '(:CHOICE "a"))
    (assert-parse-result p "b" '(:CHOICE "b"))
    (assert-parse-result p "c" '(:CHOICE "c"))))

(deftest test-concatenation
  (let ((p (parser "cat = 'a' 'b' 'c'")))
    (assert-parse-result p "abc" '(:CAT "a" "b" "c"))))

(deftest test-optional
  (let ((p (parser "opt = 'a' 'b'?")))
    (assert-parse-result p "a" '(:OPT "a"))
    (assert-parse-result p "ab" '(:OPT "a" "b"))))

(deftest test-star
  (let ((p (parser "star = 'a'*")))
    (assert-parse-result p "" '(:STAR))
    (assert-parse-result p "a" '(:STAR "a"))
    (assert-parse-result p "aaa" '(:STAR "a" "a" "a"))))

(deftest test-plus
  (let ((p (parser "plus = 'a'+")))
    (assert-parse-result p "a" '(:PLUS "a"))
    (assert-parse-result p "aaa" '(:PLUS "a" "a" "a"))))

(deftest test-nested-rules
  (let ((p (parser "
            s = word+
            word = #'[a-z]+' ' '?")))
    (assert-parse-success p "hello world")))


;;;; Hiding Tests

(deftest test-hide-literal
  (let ((p (parser "s = <'('> 'a' <')'>")))
    (assert-parse-result p "(a)" '(:S "a"))))

(deftest test-hide-rule
  (let ((p (parser "
            s = a b
            <ws> = ' '*
            a = 'a' <ws>
            b = 'b'")))
    (assert-parse-success p "a   b")))


;;;; Grammar Examples

(deftest test-arithmetic
  (let ((p (parser "
            expr = term (('+' | '-') term)*
            term = factor (('*' | '/') factor)*
            factor = number | '(' expr ')'
            number = #'[0-9]+'")))
    (assert-parse-success p "1+2*3")
    (assert-parse-success p "(1+2)*3")))

(deftest test-simple-json-string
  (let ((p (parser "
            value = string | number | 'true' | 'false' | 'null'
            string = #'\"[^\"]*\"'
            number = #'-?[0-9]+'")))
    (assert-parse-result p "\"hello\"" '(:VALUE (:STRING "\"hello\"")))
    (assert-parse-result p "123" '(:VALUE (:NUMBER "123")))
    (assert-parse-result p "true" '(:VALUE "true"))))


;;;; Lookahead Tests

(deftest test-negative-lookahead
  (let ((p (parser "
            word = !digit letter+
            letter = #'[a-zA-Z]'
            digit = #'[0-9]'")))
    (assert-parse-success p "hello")
    ;; Should fail on "123"
    (multiple-value-bind (result success-p)
        (parse p "123")
      (declare (ignore result))
      (assert-true (not success-p)))))


;;;; ABNF Tests

(deftest test-abnf-basic-rule
  (let* ((grammar (iparse/abnf:build-abnf-parser "greeting = \"Hello\""))
         (p (iparse::%make-iparser :grammar grammar :start-production :greeting)))
    (assert-parse-result p "Hello" '(:GREETING "Hello"))))

(deftest test-abnf-alternation
  (let* ((grammar (iparse/abnf:build-abnf-parser "choice = \"a\" / \"b\" / \"c\""))
         (p (iparse::%make-iparser :grammar grammar :start-production :choice)))
    (assert-parse-result p "a" '(:CHOICE "a"))
    (assert-parse-result p "b" '(:CHOICE "b"))))

(deftest test-abnf-concatenation
  (let* ((grammar (iparse/abnf:build-abnf-parser "seq = \"a\" \"b\" \"c\""))
         (p (iparse::%make-iparser :grammar grammar :start-production :seq)))
    (assert-parse-result p "abc" '(:SEQ "a" "b" "c"))))

(deftest test-abnf-repetition
  (let* ((grammar (iparse/abnf:build-abnf-parser "stars = *\"a\""))
         (p (iparse::%make-iparser :grammar grammar :start-production :stars)))
    (assert-parse-result p "" '(:STARS))
    (assert-parse-result p "aaa" '(:STARS "a" "a" "a"))))

(deftest test-abnf-numeric-value
  (let* ((grammar (iparse/abnf:build-abnf-parser "nl = %x0A"))  ; Newline
         (p (iparse::%make-iparser :grammar grammar :start-production :nl)))
    (assert-parse-success p (string #\Newline))))

(deftest test-abnf-core-rules
  (let* ((grammar (iparse/abnf:build-abnf-parser "dig = DIGIT"))  ; References ABNF core
         (p (iparse::%make-iparser :grammar grammar :start-production :dig)))
    (assert-parse-success p "5")))


;;;; Transform Tests

(deftest test-transform
  (let* ((p (parser "num = #'[0-9]+'"))
         (tree (parse p "42"))
         (transform-map (make-hash-table)))
    (setf (gethash :NUM transform-map)
          (lambda (s) (parse-integer s)))
    (let ((result (transform transform-map tree)))
      (assert-equal 42 result))))


;;;; Run All Tests

(defun run-all-tests ()
  (setf *test-count* 0
        *pass-count* 0
        *fail-count* 0)

  (format t "~%Running iparse tests...~%~%")

  ;; Basic tests
  (format t "Basic Combinators:~%")
  (test-string-parser)
  (test-regexp-parser)
  (test-alternation)
  (test-concatenation)
  (test-optional)
  (test-star)
  (test-plus)
  (test-nested-rules)

  ;; Hiding tests
  (format t "~%Hiding:~%")
  (test-hide-literal)
  (test-hide-rule)

  ;; Grammar examples
  (format t "~%Grammar Examples:~%")
  (test-arithmetic)
  (test-simple-json-string)

  ;; Lookahead tests
  (format t "~%Lookahead:~%")
  (test-negative-lookahead)

  ;; ABNF tests
  (format t "~%ABNF:~%")
  (test-abnf-basic-rule)
  (test-abnf-alternation)
  (test-abnf-concatenation)
  (test-abnf-repetition)
  (test-abnf-numeric-value)
  (test-abnf-core-rules)

  ;; Transform tests
  (format t "~%Transform:~%")
  (test-transform)

  ;; Summary
  (format t "~%~%========================================~%")
  (format t "Results: ~D/~D passed" *pass-count* *test-count*)
  (when (plusp *fail-count*)
    (format t ", ~D FAILED" *fail-count*))
  (format t "~%========================================~%~%")

  (zerop *fail-count*))
