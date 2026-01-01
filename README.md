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

iparse produces parse trees in hiccup format:

```lisp
(:TAG child1 child2 ...)
```

Where `:TAG` is a keyword matching the rule name, and children are either nested nodes or string terminals.

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
| `defparser` | Define a parser as a named function |
| `parser` | Create a parser object |
| `parse` | Parse input (string, stream, or pathname) |
| `parses` | Get lazy list of all parses (for ambiguous grammars) |
| `with-parser` | Create scoped temporary parser |
| `transform` | Transform tree with hash-table of functions |
| `transform-with-methods` | Transform using `parse-node` generic function |
| `pprint-parse-tree` | Pretty-print a parse tree |

## License

MIT License

## Acknowledgments

This is a port of [instaparse](https://github.com/Engelberg/instaparse) by Mark Engelberg.
