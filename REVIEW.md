**Findings**
- **Correctness: `:partial` parsing is ignored.** `parse` computes `total` but calls `iparse/gll:parse-grammar` in both branches, and `parse-grammar` always registers a full listener, so it requires full consumption even when `:partial t`. This makes `*parse-partial*` and the `:partial` keyword effectively no-ops. `src/core.lisp:128-155`, `src/gll.lisp:751-781`.
- **Correctness: line/column metadata cannot work with public `parse`.** `unwrap-result` strips metaobjects before returning, but `add-line-and-column-info` relies on metadata attached during parsing. As written, `get-meta` will always be NIL for nodes returned by `parse`. `src/core.lisp:114-125`, `src/transform.lisp:64-85`.
- **Correctness: `read-file-content` is unsafe for character streams/external formats.** `(file-length stream)` is not reliable on character streams (and can be NIL or wrong for multibyte external formats), which can truncate or error. `src/core.lisp:170-175`.
- **Performance: `auto-flatten-seq` concatenation is not O(1).** `conj-flat` repeatedly calls `concatenate` which copies the whole vector each append, making it O(n) per append and defeating the stated O(1) goal. `src/auto-flatten-seq.lisp:88-113`.
- **Performance: negative listener handling re-sorts on every pop.** `pop-highest-negative-listener` sorts the entire list each time; for many negative listeners this can become a hotspot. `src/gll.lisp:139-152`.
- **Best-practice: `defparser`/`defgrammar` use `defvar`, so reloading won’t update the parser.** In REPL workflows this leads to stale parsers after redefinition unless the var is unbound. `src/core.lisp:210-215`, `src/core.lisp:426-431`.

**Open questions / assumptions**
- Do you want `:partial` to return the first successful prefix parse (and which prefix if multiple), or to return the longest? This affects the intended semantics and API.
- Should public `parse` preserve metadata (or expose an option) so `add-line-and-column-info` works without using internal APIs?

**Change summary**
- No changes made.

**Testing**
- Not run (no test command requested). If you want, I can run `test/tests.lisp` and add a few targeted tests for partial parsing and metadata propagation.
