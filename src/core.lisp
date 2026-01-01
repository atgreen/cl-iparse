;;; core.lisp
;;;
;;; SPDX-License-Identifier: EPL-1.0
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; This is a Common Lisp port of instaparse by Mark Engelberg.
;;; Original: https://github.com/Engelberg/instaparse
;;; Licensed under the Eclipse Public License 1.0.
;;;
;;; Public API for iparse.
;;;
;;; Provides an idiomatic Common Lisp interface with:
;;; - Special variables for configuration
;;; - Conditions with restarts for error handling
;;; - Stream support
;;; - Pretty printing
;;; - Generic functions for extensibility

(in-package #:iparse)

;;;; Special Variables for Configuration

(defvar *signal-errors* nil
  "If T, signal PARSE-ERROR conditions instead of returning failure.
Default is NIL for compatibility with multiple-value return style.")

(defvar *parse-partial* nil
  "If T, don't require consuming all input.
Default is NIL (total parse required).")

(defvar *print-parse-tree-indent* 2
  "Number of spaces for each indentation level in pretty-printed parse trees.")

;;;; Parser Class
;;;;
;;;; A parser wraps a grammar with configuration.
;;;; It's a funcallable instance, so you can call it directly:
;;;;   (funcall parser "input text")

(defclass iparser ()
  ((grammar :initarg :grammar
            :reader iparser-grammar
            :type hash-table
            :documentation "Grammar: keyword -> parser map.")
   (start-production :initarg :start-production
                     :reader iparser-start-production
                     :type keyword
                     :documentation "Default start rule."))
  (:metaclass c2mop:funcallable-standard-class)
  (:documentation "A compiled parser created from a grammar specification.
Can be called directly as a function: (funcall parser text)"))

(defun %make-iparser (&key grammar start-production)
  "Create a funcallable parser instance."
  (let ((parser (make-instance 'iparser
                               :grammar grammar
                               :start-production start-production)))
    ;; Set the function to call when parser is funcalled
    (c2mop:set-funcallable-instance-function
     parser
     (lambda (text &key start)
       (parse parser text :start start)))
    parser))


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

(defun parse (parser input &key (start nil) (partial *parse-partial*)
                                 (signal-errors *signal-errors*))
  "Parse INPUT using PARSER.

INPUT can be a string or a stream (stream contents will be read).

Returns two values:
  - The parse result (parse tree on success, failure object on failure)
  - T on success, NIL on failure

If *SIGNAL-ERRORS* is T (or :signal-errors is T), signals PARSE-ERROR
condition on failure instead of returning the failure object.

Options:
  :start         - Override start production
  :partial       - If T, don't require consuming all input (default: *parse-partial*)
  :signal-errors - If T, signal condition on failure (default: *signal-errors*)"
  (let* ((text (etypecase input
                 (string input)
                 (stream (read-stream-content input))
                 (pathname (read-file-content input))))
         (grammar (iparser-grammar parser))
         (start-rule (or start (iparser-start-production parser)))
         (total (not partial)))
    (multiple-value-bind (result success-p)
        (if total
            (iparse/gll:parse-grammar grammar start-rule text)
            (iparse/gll:parse-grammar grammar start-rule text))
      (if success-p
          (values (unwrap-result result) t)
          (if signal-errors
              (iparse/failure:signal-parse-error result)
              (values result nil))))))

(defun read-stream-content (stream)
  "Read entire content of STREAM as a string."
  (let ((buffer (make-array 1024 :element-type 'character :adjustable t :fill-pointer 0)))
    (loop for char = (read-char stream nil nil)
          while char
          do (vector-push-extend char buffer))
    (coerce buffer 'string)))

(defun read-file-content (pathname)
  "Read entire content of file at PATHNAME as a string."
  (with-open-file (stream pathname :direction :input)
    (let ((content (make-string (file-length stream))))
      (read-sequence content stream)
      content)))

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


;;;; defparser Macro

(defmacro defparser (name grammar-spec &key start)
  "Define a parser function NAME from GRAMMAR-SPEC.

This creates a function that can be called directly:
  (defparser my-parser \"s = 'hello'\")
  (my-parser \"hello\")  ; => (:S \"hello\")

GRAMMAR-SPEC can be a string (EBNF) or a form evaluating to a hash table.

Options:
  :start - The start production (keyword). Defaults to first rule."
  (let ((parser-var (gensym "PARSER")))
    `(progn
       (defvar ,parser-var (parser ,grammar-spec :start ,start))
       (defun ,name (text &key start)
         ,(format nil "Parse TEXT using the ~A grammar.~%~%Returns two values: result and success-p." name)
         (parse ,parser-var text :start start))
       ',name)))


;;;; S-Expression Grammar DSL
;;;;
;;;; An alternative to EBNF strings using pure S-expressions.
;;;; Example:
;;;;   (defgrammar expr
;;;;     (expr   (seq term (* (seq (or "+" "-") term))))
;;;;     (term   (seq factor (* (seq (or "*" "/") factor))))
;;;;     (factor (or number (seq (hide "(") expr (hide ")"))))
;;;;     (number (regex "[0-9]+")))

(defun make-keyword-from-symbol (sym)
  "Convert symbol to keyword, handling <name> hidden rule convention."
  (let ((name (symbol-name sym)))
    (if (and (> (length name) 2)
             (char= (char name 0) #\<)
             (char= (char name (1- (length name))) #\>))
        ;; Hidden rule: <ws> -> :WS (but mark for hiding)
        (values (intern (string-upcase (subseq name 1 (1- (length name)))) :keyword)
                t)
        (values (intern (string-upcase name) :keyword)
                nil))))

(defun compile-sexpr-rule (form)
  "Compile an S-expression grammar form into a parser combinator form.

Supported forms:
  \"literal\"        - String literal
  symbol            - Non-terminal reference
  (seq a b ...)     - Concatenation
  (or a b ...)      - Alternation (unordered)
  (ord a b)         - Ordered choice
  (* x)             - Zero or more
  (+ x)             - One or more
  (? x)             - Optional
  (rep min max x)   - Bounded repetition
  (hide x)          - Hide result
  (hide-tag x)      - Hide tag only
  (look x)          - Positive lookahead
  (neg x)           - Negative lookahead
  (regex pat)       - Regular expression
  (char-range lo hi) - Character range"
  (cond
    ;; String literal
    ((stringp form)
     `(iparse/combinators:make-string-parser ,form))

    ;; Non-terminal reference (symbol)
    ((symbolp form)
     (multiple-value-bind (kw hidden) (make-keyword-from-symbol form)
       ;; If the symbol uses <name> convention, hide the reference result
       (if hidden
           `(iparse/combinators:hide (iparse/combinators:make-nt ,kw))
           `(iparse/combinators:make-nt ,kw))))

    ;; Compound forms
    ((consp form)
     (let ((op (car form))
           (args (cdr form)))
       ;; Use string= on symbol names to handle symbols from any package
       (let ((op-name (and (symbolp op) (symbol-name op))))
         (cond
           ;; Concatenation
           ((string= op-name "SEQ")
            (if (= (length args) 1)
                (compile-sexpr-rule (first args))
                `(iparse/combinators:make-cat
                  ,@(mapcar #'compile-sexpr-rule args))))

           ;; Alternation (unordered)
           ((string= op-name "OR")
            (if (= (length args) 1)
                (compile-sexpr-rule (first args))
                `(iparse/combinators:make-alt
                  ,@(mapcar #'compile-sexpr-rule args))))

           ;; Ordered choice
           ((string= op-name "ORD")
            `(iparse/combinators:make-ord
              ,(compile-sexpr-rule (first args))
              ,(compile-sexpr-rule (second args))))

           ;; Zero or more
           ((string= op-name "*")
            `(iparse/combinators:make-star
              ,(compile-sexpr-rule (first args))))

           ;; One or more
           ((string= op-name "+")
            `(iparse/combinators:make-plus
              ,(compile-sexpr-rule (first args))))

           ;; Optional
           ((string= op-name "?")
            `(iparse/combinators:make-opt
              ,(compile-sexpr-rule (first args))))

           ;; Bounded repetition
           ((string= op-name "REP")
            `(iparse/combinators:make-rep
              ,(first args)
              ,(second args)
              ,(compile-sexpr-rule (third args))))

           ;; Hide result
           ((string= op-name "HIDE")
            `(iparse/combinators:hide
              ,(compile-sexpr-rule (first args))))

           ;; Hide tag only
           ((string= op-name "HIDE-TAG")
            `(iparse/combinators:hide-tag
              ,(compile-sexpr-rule (first args))))

           ;; Positive lookahead
           ((string= op-name "LOOK")
            `(iparse/combinators:make-look
              ,(compile-sexpr-rule (first args))))

           ;; Negative lookahead
           ((string= op-name "NEG")
            `(iparse/combinators:make-neg
              ,(compile-sexpr-rule (first args))))

           ;; Regular expression
           ((string= op-name "REGEX")
            `(iparse/combinators:make-regexp-parser ,(first args)))

           ;; Character range
           ((string= op-name "CHAR-RANGE")
            `(iparse/combinators:make-char-range-parser
              ,(first args)
              ,(second args)))

           ;; Unknown operator
           (t
            (error "Unknown S-expr grammar operator: ~S" op))))))

    (t (error "Invalid S-expr grammar form: ~S" form))))

(defun compile-sexpr-grammar (rules)
  "Compile a list of (name form) rules into grammar hash-table construction code."
  (let ((rule-forms nil)
        (first-rule nil))
    (dolist (rule rules)
      (destructuring-bind (name-sym body) rule
        (multiple-value-bind (kw hidden) (make-keyword-from-symbol name-sym)
          ;; First non-hidden rule becomes the start production
          (unless (or first-rule hidden)
            (setf first-rule kw))
          (let ((compiled (compile-sexpr-rule body)))
            ;; If rule name uses <name> convention, hide its tag
            (when hidden
              (setf compiled `(iparse/combinators:hide-tag ,compiled)))
            (push `(setf (gethash ,kw grammar) ,compiled) rule-forms)))))
    (values (nreverse rule-forms) first-rule)))

(defmacro grammar (&body rules)
  "Create a parser from S-expression grammar rules.

Each rule is (name form) where name is a symbol and form uses:
  \"literal\"        - String literal
  symbol            - Non-terminal reference
  (seq a b ...)     - Concatenation
  (or a b ...)      - Alternation
  (ord a b)         - Ordered choice
  (* x)             - Zero or more
  (+ x)             - One or more
  (? x)             - Optional
  (rep min max x)   - Bounded repetition
  (hide x)          - Hide result
  (look x)          - Positive lookahead
  (neg x)           - Negative lookahead
  (regex pat)       - Regular expression
  (char-range lo hi) - Character range

Rule names starting with < and ending with > (like <ws>) are
automatically hidden (their tag won't appear in output).

Example:
  (grammar
    (expr   (seq term (* (seq (or \"+\" \"-\") term))))
    (term   (regex \"[0-9]+\")))"
  (multiple-value-bind (rule-forms first-rule)
      (compile-sexpr-grammar rules)
    `(let ((grammar (make-hash-table :test 'eq)))
       ,@rule-forms
       ;; Apply standard hiccup reductions to get (:TAG ...) output format
       (iparse/reduction:apply-standard-reductions grammar)
       (%make-iparser :grammar grammar
                      :start-production ,first-rule))))

(defmacro defgrammar (name &body rules)
  "Define a parser function NAME from S-expression grammar rules.

This creates a function that can be called directly:
  (defgrammar my-parser
    (greeting (or \"hello\" \"hi\")))
  (my-parser \"hello\")  ; => (:GREETING \"hello\")

See GRAMMAR macro for rule syntax.

Example:
  (defgrammar expr
    (expr   (seq term (* (seq (or \"+\" \"-\") term))))
    (term   (seq factor (* (seq (or \"*\" \"/\") factor))))
    (factor (or number (seq (hide \"(\") expr (hide \")\"))))
    (number (regex \"[0-9]+\")))"
  (let ((parser-var (gensym "PARSER")))
    `(progn
       (defvar ,parser-var (grammar ,@rules))
       (defun ,name (text &key start)
         ,(format nil "Parse TEXT using the ~A grammar.~%~%Returns two values: result and success-p." name)
         (parse ,parser-var text :start start))
       ',name)))


;;;; Re-export transform

(setf (fdefinition 'transform) #'iparse/transform:transform)
(setf (fdefinition 'add-line-and-column-info) #'iparse/transform:add-line-and-column-info)


;;;; WITH-PARSER Macro

(defmacro with-parser ((var grammar-spec &key start) &body body)
  "Create a parser and bind it to VAR for the duration of BODY.

Example:
  (with-parser (p \"s = 'hello'\")
    (p \"hello\"))  ; => (:S \"hello\")"
  `(let ((,var (parser ,grammar-spec :start ,start)))
     (flet ((,var (input &rest args)
              (apply #'parse ,var input args)))
       ,@body)))


;;;; Pretty Printer for Parse Trees

(defun pprint-parse-tree (tree &optional (stream *standard-output*) (indent 0))
  "Pretty-print a parse tree with indentation.

Example output:
  (:EXPR
    (:TERM
      (:NUMBER \"42\")))"
  (let ((prefix (make-string indent :initial-element #\Space)))
    (cond
      ((null tree)
       (format stream "~ANIL~%" prefix))
      ((atom tree)
       (format stream "~A~S~%" prefix tree))
      ((and (keywordp (car tree)) (listp (cdr tree)))
       ;; Parse tree node
       (format stream "~A(~S" prefix (car tree))
       (if (and (= (length (cdr tree)) 1)
                (atom (cadr tree)))
           ;; Single atomic child - keep on same line
           (format stream " ~S)~%" (cadr tree))
           ;; Multiple or complex children - indent
           (progn
             (format stream "~%")
             (dolist (child (cdr tree))
               (pprint-parse-tree child stream (+ indent *print-parse-tree-indent*)))
             (format stream "~A)~%" prefix))))
      (t
       ;; Regular list
       (format stream "~A(" prefix)
       (dolist (item tree)
         (pprint-parse-tree item stream (+ indent *print-parse-tree-indent*)))
       (format stream "~A)~%" prefix))))
  tree)

(defun parse-tree-to-string (tree)
  "Convert parse tree to a pretty-printed string."
  (with-output-to-string (s)
    (pprint-parse-tree tree s)))


;;;; Generic Functions for Extensibility

(defgeneric parse-node (tag children)
  (:documentation "Process a parse tree node during transformation.
Specialize on TAG (a keyword) to customize how nodes are processed.

Example:
  (defmethod parse-node ((tag (eql :number)) children)
    (parse-integer (first children)))"))

(defmethod parse-node ((tag t) children)
  "Default method: reconstruct the node unchanged."
  (cons tag children))

(defun transform-with-methods (tree)
  "Transform a parse tree using PARSE-NODE generic function methods.

Example:
  (defmethod parse-node ((tag (eql :number)) children)
    (parse-integer (first children)))

  (transform-with-methods '(:expr (:number \"42\")))
  ; => (:EXPR 42)"
  (cond
    ((null tree) nil)
    ((atom tree) tree)
    ((and (keywordp (car tree)) (listp (cdr tree)))
     (parse-node (car tree)
                 (mapcar #'transform-with-methods (cdr tree))))
    (t (mapcar #'transform-with-methods tree))))
