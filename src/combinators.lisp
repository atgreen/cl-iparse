;;; combinators.lisp
;;;
;;; SPDX-License-Identifier: EPL-1.0
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; This is a Common Lisp port of instaparse by Mark Engelberg.
;;; Original: https://github.com/Engelberg/instaparse
;;; Licensed under the Eclipse Public License 1.0.
;;;
;;; Parser combinator classes and constructors.
;;; Each combinator is represented as a CLOS class.

(in-package #:iparse/combinators)

;;;; Base Parser Class

(defclass parser ()
  ((hide :initarg :hide
         :initform nil
         :accessor parser-hide
         :documentation "If true, this parser's result is hidden in output.")
   (reduction :initarg :red
              :initform nil
              :accessor parser-reduction
              :documentation "Reduction function/spec to apply to results."))
  (:documentation "Base class for all parser combinators."))

(defgeneric copy-parser (parser)
  (:documentation "Return a shallow copy of PARSER."))

(defmethod copy-parser ((p parser))
  (let ((copy (allocate-instance (class-of p))))
    (dolist (slot (mapcar #'closer-mop:slot-definition-name
                          (closer-mop:class-slots (class-of p))))
      (when (slot-boundp p slot)
        (setf (slot-value copy slot) (slot-value p slot))))
    copy))


;;;; Terminal Parsers

(defclass string-parser (parser)
  ((string :initarg :string
           :reader parser-string
           :type string
           :documentation "The literal string to match."))
  (:documentation "Matches an exact string literal."))

(defun string-parser-p (x)
  (typep x 'string-parser))

(defmethod print-object ((p string-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~S" (parser-string p))))


(defclass string-ci-parser (parser)
  ((string :initarg :string
           :reader parser-string
           :type string
           :documentation "The literal string to match (case-insensitive)."))
  (:documentation "Matches a string literal case-insensitively."))

(defun string-ci-parser-p (x)
  (typep x 'string-ci-parser))

(defmethod print-object ((p string-ci-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~S (case-insensitive)" (parser-string p))))


(defclass regexp-parser (parser)
  ((pattern :initarg :pattern
            :reader parser-pattern
            :type string
            :documentation "The regex pattern string.")
   (scanner :initarg :scanner
            :reader parser-scanner
            :documentation "Compiled cl-ppcre scanner."))
  (:documentation "Matches text using a regular expression."))

(defun regexp-parser-p (x)
  (typep x 'regexp-parser))

(defmethod print-object ((p regexp-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "#~S" (parser-pattern p))))


(defclass char-range-parser (parser)
  ((lo :initarg :lo
       :reader parser-lo
       :type fixnum
       :documentation "Low end of character code range (inclusive).")
   (hi :initarg :hi
       :reader parser-hi
       :type fixnum
       :documentation "High end of character code range (inclusive)."))
  (:documentation "Matches a single character within a code point range."))

(defun char-range-parser-p (x)
  (typep x 'char-range-parser))

(defmethod print-object ((p char-range-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "[~A-~A]"
            (code-char (parser-lo p))
            (code-char (parser-hi p)))))


;;;; Composition Parsers

(defclass cat-parser (parser)
  ((parsers :initarg :parsers
            :reader parser-parsers
            :type list
            :documentation "List of parsers to match in sequence."))
  (:documentation "Concatenation: matches all parsers in sequence."))

(defun cat-parser-p (x)
  (typep x 'cat-parser))

(defmethod print-object ((p cat-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "(~{~A~^ ~})" (parser-parsers p))))


(defclass alt-parser (parser)
  ((parsers :initarg :parsers
            :reader parser-parsers
            :type list
            :documentation "List of alternative parsers (unordered)."))
  (:documentation "Alternation: tries all parsers, returns all successes."))

(defun alt-parser-p (x)
  (typep x 'alt-parser))

(defmethod print-object ((p alt-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "(~{~A~^ | ~})" (parser-parsers p))))


(defclass ord-parser (parser)
  ((parser1 :initarg :parser1
            :reader parser-parser1
            :documentation "First parser to try.")
   (parser2 :initarg :parser2
            :reader parser-parser2
            :documentation "Second parser to try if first fails."))
  (:documentation "Ordered choice: tries parser1 first, parser2 only if parser1 fails."))

(defun ord-parser-p (x)
  (typep x 'ord-parser))

(defmethod print-object ((p ord-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "(~A / ~A)" (parser-parser1 p) (parser-parser2 p))))


;;;; Quantifier Parsers

(defclass opt-parser (parser)
  ((parser :initarg :parser
           :reader parser-parser
           :documentation "Parser that may or may not match."))
  (:documentation "Optional: matches zero or one occurrence."))

(defun opt-parser-p (x)
  (typep x 'opt-parser))

(defmethod print-object ((p opt-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A?" (parser-parser p))))


(defclass plus-parser (parser)
  ((parser :initarg :parser
           :reader parser-parser
           :documentation "Parser to match one or more times."))
  (:documentation "Plus: matches one or more occurrences."))

(defun plus-parser-p (x)
  (typep x 'plus-parser))

(defmethod print-object ((p plus-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A+" (parser-parser p))))


(defclass star-parser (parser)
  ((parser :initarg :parser
           :reader parser-parser
           :documentation "Parser to match zero or more times."))
  (:documentation "Star: matches zero or more occurrences."))

(defun star-parser-p (x)
  (typep x 'star-parser))

(defmethod print-object ((p star-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A*" (parser-parser p))))


(defclass rep-parser (parser)
  ((parser :initarg :parser
           :reader parser-parser
           :documentation "Parser to repeat.")
   (min :initarg :min
        :reader parser-min
        :type fixnum
        :documentation "Minimum number of repetitions.")
   (max :initarg :max
        :reader parser-max
        :type fixnum
        :documentation "Maximum number of repetitions."))
  (:documentation "Repetition: matches between min and max occurrences."))

(defun rep-parser-p (x)
  (typep x 'rep-parser))

(defmethod print-object ((p rep-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A{~D,~D}" (parser-parser p) (parser-min p) (parser-max p))))


;;;; Reference and Lookahead Parsers

(defclass nt-parser (parser)
  ((keyword :initarg :keyword
            :reader parser-keyword
            :type keyword
            :documentation "Keyword naming the non-terminal rule."))
  (:documentation "Non-terminal: references another rule by name."))

(defun nt-parser-p (x)
  (typep x 'nt-parser))

(defmethod print-object ((p nt-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "~A" (parser-keyword p))))


(defclass look-parser (parser)
  ((parser :initarg :parser
           :reader parser-parser
           :documentation "Parser to lookahead with."))
  (:documentation "Positive lookahead: succeeds if parser matches, consumes nothing."))

(defun look-parser-p (x)
  (typep x 'look-parser))

(defmethod print-object ((p look-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "&~A" (parser-parser p))))


(defclass neg-parser (parser)
  ((parser :initarg :parser
           :reader parser-parser
           :documentation "Parser that must not match."))
  (:documentation "Negative lookahead: succeeds if parser fails, consumes nothing."))

(defun neg-parser-p (x)
  (typep x 'neg-parser))

(defmethod print-object ((p neg-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "!~A" (parser-parser p))))


;;;; Epsilon Parser (Singleton)

(defclass epsilon-parser (parser)
  ()
  (:documentation "Epsilon: matches empty string, always succeeds."))

(defun epsilon-parser-p (x)
  (typep x 'epsilon-parser))

(defmethod print-object ((p epsilon-parser) stream)
  (print-unreadable-object (p stream :type t)
    (format stream "epsilon")))

(defvar *epsilon* (make-instance 'epsilon-parser)
  "Singleton epsilon parser - matches empty string.")


;;;; Constructors
;;;;
;;;; These constructors handle edge cases like empty strings returning epsilon.

(defun make-string-parser (s)
  "Create a parser matching the literal string S."
  (if (zerop (length s))
      *epsilon*
      (make-instance 'string-parser :string s)))

(defun make-string-ci-parser (s)
  "Create a parser matching the literal string S (case-insensitive)."
  (if (zerop (length s))
      *epsilon*
      (make-instance 'string-ci-parser :string s)))

(defun make-regexp-parser (pattern)
  "Create a parser matching the regex PATTERN."
  (make-instance 'regexp-parser
                 :pattern pattern
                 :scanner (cl-ppcre:create-scanner pattern)))

(defun make-char-range-parser (lo &optional hi)
  "Create a parser matching a character with code between LO and HI."
  (make-instance 'char-range-parser
                 :lo lo
                 :hi (or hi lo)))

(defun make-cat (&rest parsers)
  "Create a concatenation of PARSERS. Filters out epsilon parsers."
  (let ((filtered (remove *epsilon* parsers)))
    (cond
      ((null filtered) *epsilon*)
      ((null (cdr filtered)) (car filtered))
      (t (make-instance 'cat-parser :parsers filtered)))))

(defun make-alt (&rest parsers)
  "Create an alternation of PARSERS."
  (cond
    ((null parsers) *epsilon*)
    ((null (cdr parsers)) (car parsers))
    ((every (lambda (p) (eq p *epsilon*)) parsers) *epsilon*)
    (t (make-instance 'alt-parser :parsers parsers))))

(defun make-ord (&rest parsers)
  "Create an ordered choice of PARSERS (right-associative)."
  (cond
    ((null parsers) *epsilon*)
    ((null (cdr parsers)) (car parsers))
    (t (reduce (lambda (p1 p2)
                 (make-instance 'ord-parser :parser1 p1 :parser2 p2))
               parsers
               :from-end t))))

(defun make-opt (parser)
  "Create an optional parser (zero or one)."
  (if (eq parser *epsilon*)
      *epsilon*
      (make-instance 'opt-parser :parser parser)))

(defun make-plus (parser)
  "Create a one-or-more parser."
  (if (eq parser *epsilon*)
      *epsilon*
      (make-instance 'plus-parser :parser parser)))

(defun make-star (parser)
  "Create a zero-or-more parser."
  (if (eq parser *epsilon*)
      *epsilon*
      (make-instance 'star-parser :parser parser)))

(defun make-rep (min max parser)
  "Create a bounded repetition parser (between MIN and MAX times)."
  (assert (<= min max))
  (cond
    ((eq parser *epsilon*) *epsilon*)
    ((and (zerop min) (zerop max)) *epsilon*)
    ((and (= min 1) (= max 1)) parser)
    (t (make-instance 'rep-parser :parser parser :min min :max max))))

(defun make-nt (name)
  "Create a non-terminal reference parser."
  (make-instance 'nt-parser
                 :keyword (if (keywordp name)
                              name
                              (intern (string-upcase (string name)) :keyword))))

(defun make-look (parser)
  "Create a positive lookahead parser."
  (make-instance 'look-parser :parser parser))

(defun make-neg (parser)
  "Create a negative lookahead parser."
  (make-instance 'neg-parser :parser parser))

(defun hide (parser)
  "Return a copy of PARSER with its result hidden in output."
  (let ((copy (copy-parser parser)))
    (setf (parser-hide copy) t)
    copy))

(defun hide-tag (parser)
  "Return a copy of PARSER with its tag hidden (results flattened into parent)."
  (let ((copy (copy-parser parser)))
    (setf (parser-reduction copy) :raw)
    copy))
