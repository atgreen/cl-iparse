;;; cfg.lisp
;;;
;;; SPDX-License-Identifier: EPL-1.0
;;;
;;; Copyright (C) 2026 Anthony Green <green@moxielogic.com>
;;;
;;; This is a Common Lisp port of instaparse by Mark Engelberg.
;;; Original: https://github.com/Engelberg/instaparse
;;; Licensed under the Eclipse Public License 1.0.
;;;
;;; CFG Parser - Parses EBNF Grammar Notation
;;;
;;; This implements a meta-grammar that parses EBNF grammars using
;;; the combinator system. It bootstraps itself: the grammar for
;;; parsing grammars is defined using raw combinators, then used
;;; to parse user-provided grammar strings.

(in-package #:iparse/cfg)

;;;; Parse Tree Navigation
;;;;
;;;; Parse trees may be wrapped in metaobjects with metadata.
;;;; These functions handle unwrapping transparently.

(defun unwrap-tree (tree)
  "Unwrap a metaobject if present, returning the underlying value."
  (if (metaobject-p tree)
      (metaobject-value tree)
      tree))

(defun tree-tag (tree)
  "Get the tag (first element) of a parse tree node."
  (let ((node (unwrap-tree tree)))
    (if (consp node) (first node) nil)))

(defun tree-contents (tree)
  "Get all children of a parse tree node."
  (let ((node (unwrap-tree tree)))
    (if (consp node) (rest node) nil)))

(defun tree-content (tree)
  "Get the first child of a parse tree node."
  (let ((node (unwrap-tree tree)))
    (if (consp node) (second node) nil)))


;;;; String Processing

(defun unescape-string (s)
  "Process escape sequences in a string literal."
  (with-output-to-string (out)
    (loop with i = 0
          while (< i (length s))
          for char = (char s i)
          do (cond ((and (char= char #\\) (< (1+ i) (length s))) (let ((next (char s (1+ i))))
                   (case next
                     (#\n (write-char #\Newline out) (incf i 2))
                     (#\t (write-char #\Tab out) (incf i 2))
                     (#\r (write-char #\Return out) (incf i 2))
                     (#\\ (write-char #\\ out) (incf i 2))
                     (#\' (write-char #\' out) (incf i 2))
                     (#\" (write-char #\" out) (incf i 2))
                     (otherwise (write-char char out) (incf i)))))
      (t
                   (write-char char out)
                   (incf i))))))

(defun process-string (s)
  "Extract and unescape a quoted string literal."
  ;; Remove surrounding quotes
  (let ((inner (subseq s 1 (1- (length s)))))
    (unescape-string inner)))

(defun process-regexp (s)
  "Extract the pattern from a regexp literal like #'pattern' or #\"pattern\"."
  ;; Format: #'pattern' or #"pattern"
  (subseq s 2 (1- (length s))))


;;;; Meta-Grammar Definition
;;;;
;;;; This defines the grammar for parsing EBNF notation.

(defun make-cfg-grammar ()
  "Build the meta-grammar for parsing EBNF notation."
  (let ((grammar (make-hash-table :test 'eq)))

    ;; Whitespace and comments
    (setf (gethash :opt-whitespace grammar)
          (hide (make-star
                 (make-alt
                  (make-regexp-parser "[ \\t\\n\\r]+")
                  (make-nt :comment)))))

    (setf (gethash :comment grammar)
          (make-cat
           (make-string-parser "(*")
           (make-nt :inside-comment)
           (make-string-parser "*)")))

    (setf (gethash :inside-comment grammar)
          (make-star
           (make-alt
            (make-regexp-parser "[^*(]+")
            (make-cat (make-string-parser "*")
                      (make-neg (make-string-parser ")")))
            (make-cat (make-string-parser "(")
                      (make-neg (make-string-parser "*")))
            (make-nt :comment))))

    ;; Top-level rules
    (setf (gethash :rules grammar)
          (hide-tag
           (make-cat
            (make-nt :opt-whitespace)
            (make-star (make-nt :rule)))))

    (setf (gethash :rule grammar)
          (make-cat
           (make-alt (make-nt :nt) (make-nt :hide-nt))
           (make-nt :opt-whitespace)
           (hide (make-nt :rule-separator))
           (make-nt :opt-whitespace)
           (make-nt :alt-or-ord)
           (hide (make-cat
                  (make-nt :opt-whitespace)
                  (make-opt (make-alt (make-string-parser ";")
                                      (make-string-parser ".")))
                  (make-nt :opt-whitespace)))))

    (setf (gethash :rule-separator grammar)
          (make-alt
           (make-string-parser "::=")
           (make-string-parser ":=")
           (make-string-parser ":")
           (make-string-parser "=")))

    ;; Non-terminals
    (setf (gethash :nt grammar)
          (make-cat
           (make-neg (make-nt :epsilon))
           (make-regexp-parser "[a-zA-Z_][a-zA-Z0-9_-]*")))

    (setf (gethash :hide-nt grammar)
          (make-cat
           (hide (make-string-parser "<"))
           (make-nt :nt)
           (hide (make-string-parser ">"))))

    ;; Alternation and ordered choice
    ;; Note: :cat is the fallback for simple expressions without | or /
    (setf (gethash :alt-or-ord grammar)
          (make-alt
           (make-nt :alt)
           (make-nt :ord)
           (make-nt :cat)))

    (setf (gethash :alt grammar)
          (make-cat
           (make-nt :cat)
           (make-plus
            (make-cat
             (make-nt :opt-whitespace)
             (hide (make-string-parser "|"))
             (make-nt :opt-whitespace)
             (make-nt :cat)))))

    (setf (gethash :ord grammar)
          (make-cat
           (make-nt :cat)
           (make-plus
            (make-cat
             (make-nt :opt-whitespace)
             (hide (make-string-parser "/"))
             (make-nt :opt-whitespace)
             (make-nt :cat)))))

    ;; Concatenation
    (setf (gethash :cat grammar)
          (make-plus
           (make-cat
            (make-nt :opt-whitespace)
            (make-nt :factor))))

    ;; Factors (primary expressions)
    (setf (gethash :factor grammar)
          (make-alt
           (make-nt :nt)
           (make-nt :string)
           (make-nt :regexp)
           (make-nt :opt)
           (make-nt :star)
           (make-nt :plus)
           (make-nt :paren)
           (make-nt :hide)
           (make-nt :look)
           (make-nt :neg)
           (make-nt :epsilon)))

    ;; Quantifiers
    (setf (gethash :opt grammar)
          (make-alt
           ;; [expr]
           (make-cat
            (hide (make-string-parser "["))
            (make-nt :opt-whitespace)
            (make-nt :alt-or-ord)
            (make-nt :opt-whitespace)
            (hide (make-string-parser "]")))
           ;; expr?
           (make-cat
            (make-nt :factor)
            (hide (make-string-parser "?")))))

    (setf (gethash :star grammar)
          (make-alt
           ;; {expr}
           (make-cat
            (hide (make-string-parser "{"))
            (make-nt :opt-whitespace)
            (make-nt :alt-or-ord)
            (make-nt :opt-whitespace)
            (hide (make-string-parser "}")))
           ;; expr*
           (make-cat
            (make-nt :factor)
            (hide (make-string-parser "*")))))

    (setf (gethash :plus grammar)
          (make-cat
           (make-nt :factor)
           (hide (make-string-parser "+"))))

    ;; Grouping
    (setf (gethash :paren grammar)
          (make-cat
           (hide (make-string-parser "("))
           (make-nt :opt-whitespace)
           (make-nt :alt-or-ord)
           (make-nt :opt-whitespace)
           (hide (make-string-parser ")"))))

    ;; Hiding
    (setf (gethash :hide grammar)
          (make-cat
           (hide (make-string-parser "<"))
           (make-nt :alt-or-ord)
           (hide (make-string-parser ">"))))

    ;; Lookahead
    (setf (gethash :look grammar)
          (make-cat
           (hide (make-string-parser "&"))
           (make-nt :factor)))

    (setf (gethash :neg grammar)
          (make-cat
           (hide (make-string-parser "!"))
           (make-nt :factor)))

    ;; Terminals
    (setf (gethash :string grammar)
          (make-alt
           (make-regexp-parser "'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'")
           (make-regexp-parser "\"[^\"\\\\]*(?:\\\\.[^\"\\\\]*)*\"")))

    (setf (gethash :regexp grammar)
          (make-alt
           (make-regexp-parser "#'[^'\\\\]*(?:\\\\.[^'\\\\]*)*'")
           (make-regexp-parser "#\"[^\"\\\\]*(?:\\\\.[^\"\\\\]*)*\"")))

    (setf (gethash :epsilon grammar)
          (make-alt
           (make-string-parser "Epsilon")
           (make-string-parser "epsilon")
           (make-string-parser "EPSILON")
           (make-string-parser "eps")
           (make-string-ci-parser "ε")))

    ;; Apply standard reductions
    (apply-standard-reductions grammar)
    grammar))

(defvar *cfg* (make-cfg-grammar)
  "The meta-grammar for parsing EBNF notation.")


;;;; Build Rule - Convert Parse Tree to Combinators

(defun build-rule (tree)
  "Convert a parse tree node to a parser combinator."
  (case (tree-tag tree)
    (:rule
     (destructuring-bind (nt-node alt-or-ord) (tree-contents tree)
       (let ((name (if (eql (tree-tag nt-node) :hide-nt)
                       (tree-content (tree-content nt-node))
                       (tree-content nt-node)))
             (parser (build-rule alt-or-ord)))
         (list (intern (string-upcase name) :keyword)
               (if (eql (tree-tag nt-node) :hide-nt)
                   (hide-tag parser)
                   parser)))))

    (:nt
     (make-nt (intern (string-upcase (tree-content tree)) :keyword)))

    (:hide-nt
     (make-nt (intern (string-upcase (tree-content (tree-content tree))) :keyword)))

    (:alt
     (apply #'make-alt (mapcar #'build-rule (tree-contents tree))))

    (:ord
     (apply #'make-ord (mapcar #'build-rule (tree-contents tree))))

    (:cat
     (apply #'make-cat (mapcar #'build-rule (tree-contents tree))))

    (:string
     (make-string-parser (process-string (tree-content tree))))

    (:regexp
     (make-regexp-parser (process-regexp (tree-content tree))))

    (:opt
     (make-opt (build-rule (tree-content tree))))

    (:star
     (make-star (build-rule (tree-content tree))))

    (:plus
     (make-plus (build-rule (tree-content tree))))

    (:paren
     (build-rule (tree-content tree)))

    (:hide
     (hide (build-rule (tree-content tree))))

    (:look
     (make-look (build-rule (tree-content tree))))

    (:neg
     (make-neg (build-rule (tree-content tree))))

    (:epsilon
     *epsilon*)

    (:alt-or-ord
     (build-rule (tree-content tree)))

    (:factor
     (build-rule (tree-content tree)))

    (otherwise
     (if (stringp tree)
         tree
         (error "Unknown parse tree node: ~A" tree)))))


;;;; Grammar Validation

(defun collect-non-terminals (parser)
  "Collect all non-terminal references in PARSER."
  (typecase parser
    (nt-parser (list (parser-keyword parser)))
    (cat-parser (mapcan #'collect-non-terminals (parser-parsers parser)))
    (alt-parser (mapcan #'collect-non-terminals (parser-parsers parser)))
    (ord-parser (append (collect-non-terminals (parser-parser1 parser))
                        (collect-non-terminals (parser-parser2 parser))))
    (opt-parser (collect-non-terminals (parser-parser parser)))
    (plus-parser (collect-non-terminals (parser-parser parser)))
    (star-parser (collect-non-terminals (parser-parser parser)))
    (rep-parser (collect-non-terminals (parser-parser parser)))
    (look-parser (collect-non-terminals (parser-parser parser)))
    (neg-parser (collect-non-terminals (parser-parser parser)))
    (otherwise nil)))

(defun validate-grammar-references (grammar)
  "Validate that all referenced non-terminals are defined in GRAMMAR.
Signals an error if any non-terminal is referenced but not defined."
  (let ((defined (alexandria:hash-table-keys grammar))
        (referenced nil))
    (maphash (lambda (key parser)
               (declare (ignore key))
               (setf referenced
                     (append referenced (collect-non-terminals parser))))
             grammar)
    (let ((undefined (set-difference (remove-duplicates referenced)
                                     defined)))
      (when undefined
        (error "Undefined non-terminals: ~{~A~^, ~}" undefined)))
    grammar))


;;;; Public Interface

(defun build-parser (grammar-string &key (start nil))
  "Build a parser from an EBNF grammar string.
Returns (values grammar start-rule)."
  (multiple-value-bind (result success-p)
      (parse-grammar *cfg* :rules grammar-string)
    (unless success-p
      (error "Failed to parse grammar:~%~A" result))

    ;; Convert AFS to list if needed, and unwrap metaobject
    (let* ((rules (cond
                    ((afs-p result) (afs-to-list result))
                    ((metaobject-p result)
                     (let ((val (metaobject-value result)))
                       (if (afs-p val) (afs-to-list val) val)))
                    (t result)))
           (grammar (make-hash-table :test 'eq))
           (first-rule nil))
      (dolist (rule-tree rules)
        (destructuring-bind (keyword parser) (build-rule rule-tree)
          (unless first-rule
            (setf first-rule keyword))
          (setf (gethash keyword grammar) parser)))

      (apply-standard-reductions grammar)
      (validate-grammar-references grammar)

      (values grammar (or start first-rule)))))

(defun ebnf (grammar-string)
  "Parse EBNF grammar string and return the grammar hash table."
  (build-parser grammar-string))
