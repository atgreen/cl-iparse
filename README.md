# iparse

A Common Lisp port of Clojure's [instaparse](https://github.com/Engelberg/instaparse) parser library.

iparse implements the GLL (Generalized LL) parsing algorithm, supporting:
- **Any context-free grammar** including left-recursive and ambiguous grammars
- **EBNF and ABNF notation** for grammar specification
- **PEG extensions** for lookahead (`&`) and negative lookahead (`!`)
- **Idiomatic Common Lisp** with conditions, restarts, streams, and generic functions

## Installation

iparse requires:
- `cl-ppcre` - Regular expressions
- `alexandria` - Common utilities
- `fset` - Functional data structures
- `closer-mop` - Portable MOP access

Load with ASDF:

```lisp
(asdf:load-system :iparse)
```

## Quick Start

### Using defparser (recommended)

```lisp
(iparse:defparser json-value "
  value = object | array | string | number | 'true' | 'false' | 'null'
  <ws> = #'\\s*'
  object = <'{'> <ws> (pair (<ws> <','> <ws> pair)*)? <ws> <'}'>
  pair = string <ws> <':'> <ws> value
  array = <'['> <ws> (value (<ws> <','> <ws> value)*)? <ws> <']'>
  string = #'\"[^\"]*\"'
  number = #'-?[0-9]+(\\.[0-9]+)?'
")

(json-value "{\"name\": \"Alice\", \"age\": 30}")
;; => (:VALUE (:OBJECT (:PAIR (:STRING "\"name\"") (:VALUE (:STRING "\"Alice\"")))
;;                     (:PAIR (:STRING "\"age\"") (:VALUE (:NUMBER "30")))))
```

### Using parser objects

```lisp
(defvar *parser* (iparse:parser "greeting = 'hello' | 'hi'"))
(iparse:parse *parser* "hello")
;; => (:GREETING "hello")

;; Or use funcall directly
(funcall *parser* "hi")
;; => (:GREETING "hi")
```

### Scoped parsers with with-parser

```lisp
(iparse:with-parser (p "num = #'[0-9]+'")
  (p "42"))
;; => (:NUM "42")
```

## EBNF Grammar Notation

```
rule = 'literal' | "literal"      ; String literals
     | #'regex'                   ; Regular expression
     | other-rule                 ; Non-terminal reference
     | rule1 rule2                ; Concatenation
     | rule1 | rule2              ; Alternation (unordered)
     | rule1 / rule2              ; Ordered choice
     | rule?                      ; Optional (zero or one)
     | rule*                      ; Zero or more
     | rule+                      ; One or more
     | [rule]                     ; Optional (alternative syntax)
     | {rule}                     ; Zero or more (alternative syntax)
     | (rule)                     ; Grouping
     | <rule>                     ; Hide result
     | &rule                      ; Positive lookahead
     | !rule                      ; Negative lookahead
     | epsilon                    ; Empty match
```

## ABNF Grammar Notation

iparse also supports RFC 5234 ABNF notation:

```lisp
(iparse/abnf:build-abnf-parser "
  greeting = hello / hi
  hello = %x48.65.6C.6C.6F   ; 'Hello' in hex
  hi = %d72.105              ; 'Hi' in decimal
")
```

ABNF features:
- Numeric values: `%x0A` (hex), `%d13` (decimal), `%b1010` (binary)
- Ranges: `%x30-39` (digits 0-9)
- Core rules: `ALPHA`, `DIGIT`, `CRLF`, `SP`, `WSP`, etc.

## S-Expression Grammar DSL

As an alternative to EBNF strings, iparse provides a pure S-expression DSL:

### Using defgrammar

```lisp
(iparse:defgrammar expr
  (expr   (seq term (* (seq (or "+" "-") term))))
  (term   (seq factor (* (seq (or "*" "/") factor))))
  (factor (or number (seq (hide "(") expr (hide ")"))))
  (number (regex "[0-9]+")))

(expr "1+2*3")
;; => (:EXPR (:TERM (:FACTOR (:NUMBER "1"))) "+"
;;           (:TERM (:FACTOR (:NUMBER "2")) "*" (:FACTOR (:NUMBER "3"))))
```

### Using grammar (anonymous parser)

```lisp
(let ((p (iparse:grammar
           (greeting (or "hello" "hi" "hey")))))
  (iparse:parse p "hello"))
;; => (:GREETING "hello")
```

### S-Expression Operators

| S-expr | EBNF | Description |
|--------|------|-------------|
| `"literal"` | `'literal'` | String literal |
| `(regex "pat")` | `#'pat'` | Regular expression |
| `symbol` | `rule-name` | Non-terminal reference |
| `(seq a b ...)` | `a b c` | Concatenation |
| `(or a b ...)` | `a \| b \| c` | Alternation |
| `(ord a b)` | `a / b` | Ordered choice |
| `(? x)` | `x?` | Optional |
| `(* x)` | `x*` | Zero or more |
| `(+ x)` | `x+` | One or more |
| `(rep n m x)` | - | Bounded repetition |
| `(hide x)` | `<x>` | Hide result |
| `(look x)` | `&x` | Positive lookahead |
| `(neg x)` | `!x` | Negative lookahead |
| `(char-range lo hi)` | - | Character range |

### Hidden Rules

Rule names wrapped in `<...>` are automatically hidden:

```lisp
(iparse:defgrammar spaced-list
  (<ws>  (regex "[ \\t]*"))           ; Hidden rule
  (list  (seq item (* (seq <ws> "," <ws> item))))
  (item  (regex "[a-z]+")))

(spaced-list "a, b, c")
;; => (:LIST (:ITEM "a") "," (:ITEM "b") "," (:ITEM "c"))
;; Note: whitespace is hidden
```

### Comparison: EBNF vs S-Expression

**EBNF string:**
```lisp
(iparse:defparser expr "
  expr = term (('+' | '-') term)*
  term = factor (('*' | '/') factor)*
  factor = number | '(' expr ')'
  number = #'[0-9]+'
")
```

**S-expression:**
```lisp
(iparse:defgrammar expr
  (expr   (seq term (* (seq (or "+" "-") term))))
  (term   (seq factor (* (seq (or "*" "/") factor))))
  (factor (or number (seq (hide "(") expr (hide ")"))))
  (number (regex "[0-9]+")))
```

Both produce identical results. Choose based on preference:
- EBNF: Compact, familiar to grammar authors
- S-expression: Full IDE support, Lisp macro integration

## Error Handling

### Multiple values (default)

```lisp
(multiple-value-bind (result success-p)
    (iparse:parse parser "bad input")
  (if success-p
      (process result)
      (format t "Error: ~A" result)))
```

### Conditions with restarts

```lisp
(let ((iparse:*signal-errors* t))
  (handler-bind ((iparse:iparse-error
                  (lambda (c)
                    (format t "Parse failed: ~A~%" c)
                    (invoke-restart 'use-value :default))))
    (my-parser "bad input")))
```

Available restarts:
- `use-value` - Return a specified value as the result
- `continue` - Return NIL and continue

### Detailed error messages

```
Parse error at line 1, column 11:
12 + 34 + bad
          ^
expected match for [0-9]+
```

## Parsing from Streams and Files

```lisp
;; Parse from stream
(with-open-file (s "data.txt")
  (iparse:parse parser s))

;; Parse from pathname
(iparse:parse parser #p"data.txt")

;; Parse from string (default)
(iparse:parse parser "input text")
```

## Configuration

Special variables control parsing behavior:

```lisp
;; Signal conditions instead of returning failure
(let ((iparse:*signal-errors* t))
  (my-parser "input"))

;; Allow partial parses (don't require consuming all input)
(let ((iparse:*parse-partial* t))
  (my-parser "hello world"))

;; Control pretty-print indentation
(let ((iparse:*print-parse-tree-indent* 4))
  (iparse:pprint-parse-tree tree))
```

## Pretty Printing

```lisp
(iparse:pprint-parse-tree '(:EXPR (:TERM (:NUMBER "42")) "+" (:TERM (:NUMBER "5"))))
;; (:EXPR
;;   (:TERM
;;     (:NUMBER "42")
;;   )
;;   "+"
;;   (:TERM
;;     (:NUMBER "5")
;;   )
;; )

;; Get as string
(iparse:parse-tree-to-string tree)
```

## Transforming Results

### Using transform maps

```lisp
(let ((transforms (make-hash-table)))
  (setf (gethash :NUMBER transforms)
        (lambda (s) (parse-integer s)))
  (iparse:transform transforms '(:EXPR (:NUMBER "42"))))
;; => (:EXPR 42)
```

### Using generic functions

```lisp
(defmethod iparse:parse-node ((tag (eql :number)) children)
  (parse-integer (first children)))

(defmethod iparse:parse-node ((tag (eql :string)) children)
  (string-trim "\"" (first children)))

(iparse:transform-with-methods '(:VALUE (:NUMBER "42")))
;; => (:VALUE 42)
```

## Programmatic Grammar Building

```lisp
(iparse:string-parser "literal")
(iparse:string-ci-parser "case-insensitive")
(iparse:regexp "pattern")
(iparse:char-range 48 57)           ; Digits 0-9
(iparse:cat parser1 parser2 ...)
(iparse:alt parser1 parser2 ...)
(iparse:ord parser1 parser2 ...)    ; Ordered choice
(iparse:opt parser)                  ; Optional
(iparse:plus parser)                 ; One or more
(iparse:star parser)                 ; Zero or more
(iparse:rep min max parser)          ; Bounded repetition
(iparse:nt :rule-name)               ; Non-terminal reference
(iparse:look parser)                 ; Positive lookahead
(iparse:neg parser)                  ; Negative lookahead
(iparse:hide parser)                 ; Hide result
(iparse:hide-tag parser)             ; Hide tag (flatten into parent)
```

## Output Format

iparse produces parse trees as tagged lists:

```lisp
(:TAG child1 child2 ...)
```

Each node is a list where the first element is a keyword tag matching the rule name, followed by children which are either nested nodes or string terminals.

## Hiding

Use `<...>` in grammars to hide results:

- `<'literal'>` - Hide this literal from output
- `<rule>` - Hide this rule's results (flatten into parent)
- `<rule-name> = ...` - Define rule but hide its tag in output

Example:
```lisp
(iparse:defparser expr "
  expr = term (<'+'> term)*
  term = #'[0-9]+'
")
(expr "1+2+3")
;; => (:EXPR (:TERM "1") (:TERM "2") (:TERM "3"))
;; Note: '+' literals are hidden
```

## API Summary

| Function/Macro | Description |
|----------------|-------------|
| `defparser` | Define a parser from EBNF string |
| `defgrammar` | Define a parser from S-expression grammar |
| `parser` | Create a parser from EBNF string |
| `grammar` | Create a parser from S-expression grammar |
| `parse` | Parse input (string, stream, or pathname) |
| `parses` | Get lazy list of all parses (for ambiguous grammars) |
| `with-parser` | Create scoped temporary parser |
| `transform` | Transform tree with hash-table of functions |
| `transform-with-methods` | Transform using `parse-node` generic function |
| `pprint-parse-tree` | Pretty-print a parse tree |

## Author

Anthony Green <green@moxielogic.com>

## License

Eclipse Public License 1.0 (same as instaparse)

## Acknowledgments

This is a Common Lisp port of [instaparse](https://github.com/Engelberg/instaparse) by Mark Engelberg.
