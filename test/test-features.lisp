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

;; Test S-expression grammar DSL
(format t "~%7. S-expression grammar (defgrammar):~%")
(iparse:defgrammar sexpr-greeting
  (greeting (or "hello" "hi" "hey")))
(format t "   (sexpr-greeting \"hello\") => ~S~%" (sexpr-greeting "hello"))
(format t "   (sexpr-greeting \"hey\") => ~S~%" (sexpr-greeting "hey"))

;; Test more complex S-expr grammar with seq, *, hide
(format t "~%8. Complex S-expression grammar:~%")
(iparse:defgrammar sexpr-expr
  (expr   (seq term (* (seq (or "+" "-") term))))
  (term   (seq factor (* (seq (or "*" "/") factor))))
  (factor (or number (seq (hide "(") expr (hide ")"))))
  (number (regex "[0-9]+")))
(format t "   (sexpr-expr \"1+2\") => ~S~%" (sexpr-expr "1+2"))
(format t "   (sexpr-expr \"3*4+5\") => ~S~%" (sexpr-expr "3*4+5"))
(format t "   (sexpr-expr \"(1+2)*3\") => ~S~%" (sexpr-expr "(1+2)*3"))

;; Test grammar macro (anonymous parser)
(format t "~%9. Anonymous grammar macro:~%")
(let ((p (iparse:grammar
           (word (+ (char-range 97 122))))))  ; a-z
  (format t "   Parsing 'hello': ~S~%" (iparse:parse p "hello")))

;; Test hidden rules with <name> convention
(format t "~%10. Hidden rules (<ws>):~%")
(iparse:defgrammar spaced-list
  (<ws>  (regex "[ \\t]*"))
  (list  (seq item (* (seq <ws> "," <ws> item))))
  (item  (regex "[a-z]+")))
(format t "   (spaced-list \"a, b, c\") => ~S~%" (spaced-list "a, b, c"))

(format t "~%=== All Features Working ===~%")
