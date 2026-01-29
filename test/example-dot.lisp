;;; example-dot.lisp
;;;
;;; Complete parser for DOT graph description language (Graphviz)
;;;
;;; Implements the full DOT language specification from:
;;; https://graphviz.org/doc/info/lang.html
;;;
;;; Supported features:
;;; - strict graphs
;;; - Directed (digraph) and undirected (graph) graphs
;;; - Subgraphs (including cluster subgraphs and anonymous subgraphs)
;;; - Node declarations with attributes and ports
;;; - Edge declarations with chaining (A -> B -> C)
;;; - Subgraphs in edge statements
;;; - Port specifications with compass points
;;; - Multiple attribute lists
;;; - Semicolon and comma separators in attributes
;;; - ID = ID assignments
;;; - HTML labels <...>
;;; - String concatenation
;;; - Comments (// /* */ #)
;;; - Case-insensitive keywords
;;; - Escape sequences in strings

(defpackage #:iparse/examples/dot
  (:use #:cl #:iparse)
  (:export #:dot-parser
           #:parse-dot-file
           #:run-dot-examples
           #:test-dot-parser))

(in-package #:iparse/examples/dot)

;;; Complete DOT Grammar
;;;
;;; Based on the official specification:
;;; graph       : [ strict ] ( graph | digraph ) [ ID ] '{' stmt_list '}'
;;; stmt_list   : [ stmt [ ';' ] stmt_list ]
;;; stmt        : node_stmt | edge_stmt | attr_stmt | ID '=' ID | subgraph
;;; attr_stmt   : ( graph | node | edge ) attr_list
;;; attr_list   : '[' [ a_list ] ']' [ attr_list ]
;;; a_list      : ID '=' ID [ ( ';' | ',' ) ] [ a_list ]
;;; edge_stmt   : ( node_id | subgraph ) edgeRHS [ attr_list ]
;;; edgeRHS     : edgeop ( node_id | subgraph ) [ edgeRHS ]
;;; node_stmt   : node_id [ attr_list ]
;;; node_id     : ID [ port ]
;;; port        : ':' ID [ ':' compass_pt ] | ':' compass_pt
;;; subgraph    : [ subgraph [ ID ] ] '{' stmt_list '}'
;;; compass_pt  : n | ne | e | se | s | sw | w | nw | c | _

(defparameter *dot-grammar*
  "
  (* Top-level graph *)
  graph = <ws> strict? <ws> graph-type <ws> id? <ws> <'{'> <ws> stmt-list <ws> <'}'> <ws>

  (* Keywords are case-insensitive - match common patterns *)
  strict = #'[Ss][Tt][Rr][Ii][Cc][Tt]'
  graph-type = #'[Dd][Ii][Gg][Rr][Aa][Pp][Hh]' | #'[Gg][Rr][Aa][Pp][Hh]'

  (* Statement list - semicolons are optional separators *)
  stmt-list = <ws> (stmt <ws> <';'>? <ws>)*

  (* Statements - use ordered choice, assignment before node-stmt to avoid ambiguity *)
  stmt = attr-stmt / subgraph / edge-stmt / assignment / node-stmt

  (* Assignment: ID = ID *)
  assignment = id <ws> <'='> <ws> id

  (* Node statement *)
  node-stmt = node-id <ws> attr-list*

  (* Edge statement with chaining *)
  edge-stmt = edge-start <ws> edge-rhs <ws> attr-list*
  edge-start = node-id | subgraph
  edge-rhs = edge-op <ws> (node-id | subgraph) (<ws> edge-op <ws> (node-id | subgraph))*
  edge-op = '->' | '--'

  (* Node ID with optional port *)
  node-id = id port?
  port = <':'> id (<':'> compass-pt)? | <':'> compass-pt
  compass-pt = 'n' | 'ne' | 'e' | 'se' | 's' | 'sw' | 'w' | 'nw' | 'c' | '_'

  (* Attribute statement *)
  attr-stmt = attr-target <ws> attr-list+
  attr-target = #'[Gg][Rr][Aa][Pp][Hh]' | #'[Nn][Oo][Dd][Ee]' | #'[Ee][Dd][Gg][Ee]'

  (* Attribute lists - can be chained *)
  attr-list = <'['> <ws> a-list? <ws> <']'>
  a-list = attr (<ws> attr-sep <ws> attr)* (<ws> attr-sep)?
  attr-sep = ',' | ';'
  attr = id <ws> <'='> <ws> id

  (* Subgraph *)
  subgraph = subgraph-keyword <ws> id? <ws> <'{'> <ws> stmt-list <ws> <'}'> | anon-subgraph
  subgraph-keyword = #'[Ss][Uu][Bb][Gg][Rr][Aa][Pp][Hh]'
  anon-subgraph = <'{'> <ws> stmt-list <ws> <'}'>

  (* IDs - identifiers, numbers, strings, HTML *)
  id = html-string | string-concat | quoted-string | number | identifier

  (* String concatenation *)
  string-concat = quoted-string (<ws> <'+'> <ws> quoted-string)+

  (* HTML labels *)
  html-string = #'<[^<>]*(?:<[^<>]*>[^<>]*)*>'

  (* Regular identifier *)
  identifier = #'[a-zA-Z_\\x80-\\xFF][a-zA-Z0-9_\\x80-\\xFF]*'

  (* Quoted string with escape sequences *)
  quoted-string = #'\"(?:[^\\\\\"]|\\\\.)*\"'

  (* Numbers: integers, decimals, with optional signs *)
  number = #'[-+]?(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)'

  (* Whitespace with comments *)
  <ws> = #'(?:[ \\t\\n\\r]|//[^\\n]*\\n|/\\*(?:[^*]|\\*[^/])*\\*/|#[^\\n]*\\n)*'
  ")

(defparameter dot-parser
  (parser *dot-grammar*)
  "Parser for complete DOT graph description language")


;;; Helper Functions

(defun parse-float (string)
  "Parse a floating point number from string"
  (let ((*read-default-float-format* 'double-float))
    (with-standard-io-syntax
      (read-from-string string))))

(defun unescape-string (str)
  "Process escape sequences in a quoted string"
  ;; Remove surrounding quotes first
  (let ((unquoted (subseq str 1 (1- (length str)))))
    ;; Process common escape sequences
    (with-output-to-string (out)
      (loop with i = 0
            while (< i (length unquoted))
            do (let ((c (char unquoted i)))
                 (if (and (char= c #\\) (< (1+ i) (length unquoted)))
                     (let ((next (char unquoted (1+ i))))
                       (case next
                         (#\n (write-char #\Newline out))
                         (#\t (write-char #\Tab out))
                         (#\r (write-char #\Return out))
                         (#\" (write-char #\" out))
                         (#\\ (write-char #\\ out))
                         (t (write-char next out)))
                       (incf i 2))
                     (progn
                       (write-char c out)
                       (incf i))))))))

(defun normalize-keyword (str)
  "Normalize a keyword to lowercase symbol"
  (intern (string-upcase str) :keyword))


;;; Transform Functions
;;;
;;; Convert parse tree to a more useful data structure

(defun transform-dot-tree (parse-tree)
  "Transform DOT parse tree into a structured representation"
  (let ((transforms (make-hash-table)))

    ;; Extract string value from identifier
    (setf (gethash :IDENTIFIER transforms)
          (lambda (&rest children)
            (first children)))

    ;; Parse numbers
    (setf (gethash :NUMBER transforms)
          (lambda (&rest children)
            (let ((num-str (first children)))
              (or (parse-integer num-str :junk-allowed t)
                  (parse-float num-str)))))

    ;; Process quoted strings with escapes
    (setf (gethash :QUOTED-STRING transforms)
          (lambda (&rest children)
            (unescape-string (first children))))

    ;; String concatenation
    (setf (gethash :STRING-CONCAT transforms)
          (lambda (&rest children)
            ;; Concatenate all the strings
            (apply #'concatenate 'string children)))

    ;; HTML strings - remove outer < >
    (setf (gethash :HTML-STRING transforms)
          (lambda (&rest children)
            (let ((html (first children)))
              (list :html (subseq html 1 (1- (length html)))))))

    ;; ID just passes through the transformed child
    (setf (gethash :ID transforms)
          (lambda (&rest children)
            (first children)))

    ;; Strict keyword
    (setf (gethash :STRICT transforms)
          (lambda (&rest children)
            (declare (ignore children))
            t))

    ;; Extract graph type (digraph or graph)
    (setf (gethash :GRAPH-TYPE transforms)
          (lambda (&rest children)
            (normalize-keyword (first children))))

    ;; Edge operator
    (setf (gethash :EDGE-OP transforms)
          (lambda (&rest children)
            (intern (first children) :keyword)))

    ;; Compass point
    (setf (gethash :COMPASS-PT transforms)
          (lambda (&rest children)
            (intern (string-upcase (first children)) :keyword)))

    ;; Port specification
    (setf (gethash :PORT transforms)
          (lambda (&rest children)
            (cond
              ;; :id:compass
              ((= (length children) 2)
               (list :port (first children) :compass (second children)))
              ;; :id or :compass
              ((= (length children) 1)
               (let ((child (first children)))
                 (if (keywordp child)
                     (list :compass child)
                     (list :port child))))
              (t nil))))

    ;; Node ID with optional port
    (setf (gethash :NODE-ID transforms)
          (lambda (&rest children)
            (if (second children)
                (list :node-id (first children) (second children))
                (list :node-id (first children)))))

    ;; Attribute separator (ignored in transform)
    (setf (gethash :ATTR-SEP transforms)
          (lambda (&rest children)
            (declare (ignore children))
            nil))

    ;; Build attribute as (key . value) pair
    (setf (gethash :ATTR transforms)
          (lambda (&rest children)
            (cons (first children) (second children))))

    ;; Build a-list (attribute list content)
    (setf (gethash :A-LIST transforms)
          (lambda (&rest children)
            (remove nil children)))

    ;; Build attribute list
    (setf (gethash :ATTR-LIST transforms)
          (lambda (&rest children)
            (first children)))  ; a-list or nil

    ;; Attribute target
    (setf (gethash :ATTR-TARGET transforms)
          (lambda (&rest children)
            (normalize-keyword (first children))))

    ;; Build attribute statement
    (setf (gethash :ATTR-STMT transforms)
          (lambda (&rest children)
            (list :attr-stmt
                  (first children)      ; target (:graph, :node, :edge)
                  (remove nil (rest children))))) ; attribute lists

    ;; Build node statement
    (setf (gethash :NODE-STMT transforms)
          (lambda (&rest children)
            (list :node-stmt
                  (first children)      ; node-id
                  (remove nil (rest children))))) ; attribute lists

    ;; Assignment statement
    (setf (gethash :ASSIGNMENT transforms)
          (lambda (&rest children)
            (list :assignment
                  (first children)
                  (second children))))

    ;; Edge start (node-id or subgraph)
    (setf (gethash :EDGE-START transforms)
          (lambda (&rest children)
            (first children)))

    ;; Edge RHS - list of (op target) pairs
    (setf (gethash :EDGE-RHS transforms)
          (lambda (&rest children)
            ;; children = (op target op target ...)
            ;; Group into pairs
            (loop for (op target) on children by #'cddr
                  while target
                  collect (list op target))))

    ;; Build edge statement
    (setf (gethash :EDGE-STMT transforms)
          (lambda (&rest children)
            ;; children = (edge-start edge-rhs-pairs attr-list*)
            (let ((start (first children))
                  (edges (second children))
                  (attrs (remove nil (cddr children))))
              (list :edge-stmt start edges attrs))))

    ;; Subgraph keyword
    (setf (gethash :SUBGRAPH-KEYWORD transforms)
          (lambda (&rest children)
            (declare (ignore children))
            :subgraph))

    ;; Anonymous subgraph
    (setf (gethash :ANON-SUBGRAPH transforms)
          (lambda (&rest children)
            (list :subgraph :anonymous nil children)))

    ;; Subgraph
    (setf (gethash :SUBGRAPH transforms)
          (lambda (&rest children)
            ;; children = (:subgraph [id] stmt-list)
            (let ((has-id (and (second children)
                              (not (listp (second children)))))
                  (id (when (and (second children)
                                (not (listp (second children))))
                       (second children)))
                  (stmts (if (and (second children)
                                 (not (listp (second children))))
                            (cddr children)
                            (cdr children))))
              (list :subgraph
                    (if has-id :named :anonymous)
                    id
                    stmts))))

    ;; Build statement - just unwrap
    (setf (gethash :STMT transforms)
          (lambda (&rest children)
            (first children)))

    ;; Build statement list
    (setf (gethash :STMT-LIST transforms)
          (lambda (&rest children)
            (remove nil children)))

    ;; Build complete graph
    (setf (gethash :GRAPH transforms)
          (lambda (&rest children)
            ;; children = ([strict] graph-type [id] stmt-list)
            (let* ((idx 0)
                   (is-strict (and (first children) (eq (first children) t)))
                   (graph-type (nth (if is-strict (incf idx) idx) children))
                   (maybe-id (nth (incf idx) children))
                   (has-id (and maybe-id (not (listp maybe-id))))
                   (graph-id (when has-id maybe-id))
                   (stmts (nth (if has-id (incf idx) idx) children)))
              (list :graph
                    :strict is-strict
                    :type graph-type
                    :id graph-id
                    :statements stmts))))

    (transform transforms parse-tree)))


;;; High-level API

(defun parse-dot-file (input)
  "Parse DOT input and return structured representation.
   INPUT can be a string, pathname, or stream."
  (multiple-value-bind (result success-p)
      (parse dot-parser input)
    (if success-p
        (values (transform-dot-tree result) t)
        (values result nil))))


;;; Examples

(defun run-dot-examples ()
  "Run example DOT parsings with output"
  (format t "~%Complete DOT Parser Examples~%")
  (format t "============================~%~%")

  ;; Example 1: Simple directed graph
  (format t "Example 1: Simple directed graph~%")
  (let* ((dot-string "digraph G {
    A -> B;
    B -> C;
    A -> C;
  }")
         (result (parse-dot-file dot-string)))
    (format t "Input:~%~A~%~%" dot-string)
    (format t "Parsed structure:~%")
    (pprint result)
    (format t "~%~%"))

  ;; Example 2: Chained edges
  (format t "Example 2: Chained edges~%")
  (let* ((dot-string "digraph Chain {
    A -> B -> C -> D;
  }")
         (result (parse-dot-file dot-string)))
    (format t "Input:~%~A~%~%" dot-string)
    (format t "Parsed structure:~%")
    (pprint result)
    (format t "~%~%"))

  ;; Example 3: Subgraphs
  (format t "Example 3: Subgraphs and clusters~%")
  (let* ((dot-string "digraph G {
    subgraph cluster_0 {
      A -> B;
    }
    subgraph cluster_1 {
      C -> D;
    }
    B -> C;
  }")
         (result (parse-dot-file dot-string)))
    (format t "Input:~%~A~%~%" dot-string)
    (format t "Parsed structure:~%")
    (pprint result)
    (format t "~%~%"))

  ;; Example 4: Ports and compass points
  (format t "Example 4: Ports and compass points~%")
  (let* ((dot-string "digraph Ports {
    node1:port1 -> node2:port2:ne;
    node3:s -> node4:n;
  }")
         (result (parse-dot-file dot-string)))
    (format t "Input:~%~A~%~%" dot-string)
    (format t "Parsed structure:~%")
    (pprint result)
    (format t "~%~%"))

  ;; Example 5: HTML labels
  (format t "Example 5: HTML labels~%")
  (let* ((dot-string "digraph HTML {
    A [label=<Bold <B>text</B>>];
    B [label=<Table <TABLE><TR><TD>Cell</TD></TR></TABLE>>];
    A -> B;
  }")
         (result (parse-dot-file dot-string)))
    (format t "Input:~%~A~%~%" dot-string)
    (format t "Parsed structure:~%")
    (pprint result)
    (format t "~%~%"))

  ;; Example 6: Comments and strict
  (format t "Example 6: Comments and strict keyword~%")
  (let* ((dot-string "strict digraph G {
    // This is a line comment
    A -> B; /* Block comment */
    A -> B; // Duplicate edge prevented by strict
    # Preprocessor-style comment
    A -> C;
  }")
         (result (parse-dot-file dot-string)))
    (format t "Input:~%~A~%~%" dot-string)
    (format t "Parsed structure:~%")
    (pprint result)
    (format t "~%~%"))

  ;; Example 7: Multiple attribute lists and separators
  (format t "Example 7: Multiple attribute lists~%")
  (let* ((dot-string "digraph Attrs {
    A [color=red][shape=box][style=filled];
    B [color=blue; shape=circle; style=dotted];
    A -> B;
  }")
         (result (parse-dot-file dot-string)))
    (format t "Input:~%~A~%~%" dot-string)
    (format t "Parsed structure:~%")
    (pprint result)
    (format t "~%~%"))

  ;; Example 8: ID assignments
  (format t "Example 8: ID assignments~%")
  (let* ((dot-string "digraph Assign {
    size=\"4,4\";
    ratio=fill;
    A -> B;
  }")
         (result (parse-dot-file dot-string)))
    (format t "Input:~%~A~%~%" dot-string)
    (format t "Parsed structure:~%")
    (pprint result)
    (format t "~%~%"))

  ;; Example 9: String concatenation
  (format t "Example 9: String concatenation~%")
  (let* ((dot-string "digraph Concat {
    A [label=\"First \" + \"Second\" + \" Third\"];
  }")
         (result (parse-dot-file dot-string)))
    (format t "Input:~%~A~%~%" dot-string)
    (format t "Parsed structure:~%")
    (pprint result)
    (format t "~%~%"))

  ;; Example 10: Complex real-world example
  (format t "Example 10: Complex graph~%")
  (let* ((dot-string "strict digraph FSM {
    rankdir=LR;
    size=\"8,5\";

    node [shape=doublecircle]; S0 S3;
    node [shape=circle];

    S0 -> S1 [label=\"a\"];
    S1 -> S2 [label=\"b\"];
    S2 -> S3 [label=\"c\"];
    S3 -> S0 [label=\"d\"];
  }")
         (result (parse-dot-file dot-string)))
    (format t "Input:~%~A~%~%" dot-string)
    (format t "Parsed structure:~%")
    (pprint result)
    (format t "~%~%"))

  (format t "~%Examples complete!~%")
  (format t "~%To use the parser in your code:~%")
  (format t "  (iparse/examples/dot:parse-dot-file \"digraph G { A -> B; }\")~%~%"))


;;; Testing

(defun test-dot-parser ()
  "Test suite for the DOT parser"
  (let ((test-cases
          '(;; Basic
            ("digraph G { A -> B; }" . t)
            ("graph G { A -- B; }" . t)

            ;; Strict
            ("strict digraph G { A -> B; }" . t)

            ;; Chained edges
            ("digraph { A -> B -> C; }" . t)
            ("digraph { A -> B -> C -> D -> E; }" . t)

            ;; Subgraphs
            ("digraph { subgraph cluster_0 { A; } }" . t)
            ("digraph { { A; B; } }" . t)

            ;; Ports
            ("digraph { A:port1 -> B:port2; }" . t)
            ("digraph { A:n -> B:s; }" . t)
            ("digraph { A:port:ne -> B; }" . t)

            ;; Attributes
            ("digraph G { A [label=\"test\"]; }" . t)
            ("digraph G { A [a=1][b=2]; }" . t)
            ("digraph G { A [a=1; b=2]; }" . t)
            ("digraph G { A [a=1, b=2, c=3]; }" . t)

            ;; HTML labels
            ("digraph { A [label=<HTML>]; }" . t)

            ;; Comments
            ("digraph { // comment
 A -> B; }" . t)
            ("digraph { /* comment */ A -> B; }" . t)
            ("digraph { # comment
 A -> B; }" . t)

            ;; Assignments
            ("digraph { size=\"4,4\"; A -> B; }" . t)

            ;; String concat
            ("digraph { A [label=\"a\"+\"b\"]; }" . t)

            ;; Case insensitive
            ("DIGRAPH G { A -> B; }" . t)
            ("DiGraph G { A -> B; }" . t)
            ("STRICT DIGRAPH { A -> B; }" . t))))

    (format t "~%Testing DOT parser...~%")
    (let ((pass 0) (fail 0))
      (dolist (test test-cases)
        (let ((input (car test))
              (should-parse (cdr test)))
          (multiple-value-bind (result success-p)
              (parse dot-parser input)
            (declare (ignore result))
            (if (eq should-parse success-p)
                (progn
                  (format t "  ~:[FAIL~;PASS~]: ~S~%" t input)
                  (incf pass))
                (progn
                  (format t "  ~:[FAIL~;PASS~]: ~S (expected ~:[fail~;success~], got ~:[fail~;success~])~%"
                          nil input should-parse success-p)
                  (incf fail))))))
      (format t "~%Results: ~D passed, ~D failed~%~%" pass fail)
      (zerop fail))))

;;; Usage:
;;;
;;; Load this file and run:
;;;   (iparse/examples/dot:run-dot-examples)
;;;   (iparse/examples/dot:test-dot-parser)
;;;
;;; Or parse your own DOT strings:
;;;   (iparse/examples/dot:parse-dot-file "digraph G { A -> B; }")
