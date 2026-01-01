;;; test-features.lisp
;;; Test new Lispy features

(in-package #:cl-user)

(format t "~%=== Testing New Features ===~%")

;; Test with-parser
(format t "~%1. with-parser macro:~%")
(iparse:with-parser (p "s = 'hello'")
  (format t "   (p \"hello\") => ~S~%" (p "hello")))

;; Test pretty printer
(format t "~%2. Pretty printer:~%")
(let ((tree '(:EXPR (:TERM (:NUMBER "42")) "+" (:TERM (:NUMBER "5")))))
  (iparse:pprint-parse-tree tree))

;; Test special variables with condition/restart
(format t "~%3. Condition with restart:~%")
(let ((iparse:*signal-errors* t))
  (handler-bind ((iparse:iparse-error
                  (lambda (c)
                    (declare (ignore c))
                    (format t "   Caught iparse-error, invoking USE-VALUE restart~%")
                    (invoke-restart 'use-value :fallback-value))))
    (let ((result (iparse:parse (iparse:parser "s = 'hello'") "bad")))
      (format t "   Result: ~S~%" result))))

;; Test defparser
(format t "~%4. defparser macro:~%")
(iparse:defparser greeting-parser "greeting = 'hello' | 'hi'")
(format t "   (greeting-parser \"hello\") => ~S~%" (greeting-parser "hello"))
(format t "   (greeting-parser \"hi\") => ~S~%" (greeting-parser "hi"))

;; Test stream parsing
(format t "~%5. Stream parsing:~%")
(with-input-from-string (s "hello")
  (let ((p (iparse:parser "s = 'hello'")))
    (format t "   Parsing from stream: ~S~%" (iparse:parse p s))))

;; Test generic function
(format t "~%6. Generic function extensibility:~%")
(defmethod iparse:parse-node ((tag (eql :number)) children)
  (parse-integer (first children)))
(let ((tree '(:expr (:number "42"))))
  (format t "   Original: ~S~%" tree)
  (format t "   Transformed: ~S~%" (iparse:transform-with-methods tree)))

(format t "~%=== All Features Working ===~%")
