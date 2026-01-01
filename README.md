# iparse

A Common Lisp port of Clojure's [instaparse](https://github.com/Engelberg/instaparse) parser library.

iparse implements the GLL (Generalized LL) parsing algorithm, supporting:
- **Any context-free grammar** including left-recursive and ambiguous grammars
- **EBNF notation** for grammar specification
- **PEG extensions** for lookahead (`&`) and negative lookahead (`!`)
- **Efficient O(n) parsing** via AutoFlattenSeq optimization

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

```lisp
(defvar *json*
  (iparse:parser "
    value = object | array | string | number | 'true' | 'false' | 'null'
    <ws> = #'\\s*'
    object = <'{'> <ws> (pair (<ws> <','> <ws> pair)*)? <ws> <'}'>
    pair = string <ws> <':'> <ws> value
    array = <'['> <ws> (value (<ws> <','> <ws> value)*)? <ws> <']'>
    string = #'\"[^\"]*\"'
    number = #'-?[0-9]+(\\.[0-9]+)?'
  "))

(iparse:parse *json* "{\"name\": \"Alice\", \"age\": 30}")
;; => (:VALUE (:OBJECT (:PAIR (:STRING "\"name\"") (:VALUE (:STRING "\"Alice\"")))
;;                     (:PAIR (:STRING "\"age\"") (:VALUE (:NUMBER "30")))))
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

## API Reference

### Creating Parsers

```lisp
(iparse:parser grammar-string &key start)
```

Create a parser from an EBNF grammar string. `:start` optionally specifies the start rule (defaults to first rule).

### Parsing

```lisp
(iparse:parse parser text &key start)
```

Parse text, returning the first successful parse or a failure object.

```lisp
(iparse:parses parser text &key start)
```

Parse text, returning a lazy list of all possible parses (useful for ambiguous grammars).

### Programmatic Grammar Building

```lisp
(iparse:string-parser "literal")
(iparse:regexp "pattern")
(iparse:cat parser1 parser2 ...)
(iparse:alt parser1 parser2 ...)
(iparse:ord parser1 parser2 ...)   ; Ordered choice
(iparse:opt parser)                 ; Optional
(iparse:plus parser)                ; One or more
(iparse:star parser)                ; Zero or more
(iparse:rep min max parser)         ; Bounded repetition
(iparse:nt :rule-name)              ; Non-terminal reference
(iparse:look parser)                ; Positive lookahead
(iparse:neg parser)                 ; Negative lookahead
(iparse:hide parser)                ; Hide result
(iparse:hide-tag parser)            ; Hide tag (flatten into parent)
```

### Post-Processing

```lisp
(iparse:transform transform-map parse-tree)
```

Transform a parse tree using functions mapped to tags.

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

## License

MIT License

## Acknowledgments

This is a port of [instaparse](https://github.com/Engelberg/instaparse) by Mark Engelberg.
