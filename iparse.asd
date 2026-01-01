;;; iparse.asd
;;;
;;; SPDX-License-Identifier: MIT
;;;
;;; Copyright (C) 2026 Your Name

(asdf:defsystem #:iparse
  :description "A Common Lisp port of Clojure's instaparse parser library.
Implements the GLL (Generalized LL) parsing algorithm with support for
ambiguous and left-recursive grammars, EBNF notation, and efficient parsing."
  :author      "Your Name"
  :license     "MIT"
  :version     "0.1.0"
  :depends-on  (#:cl-ppcre
                #:alexandria
                #:fset
                #:closer-mop)
  :serial t
  :components ((:module "src"
                :components
                ((:file "packages")
                 (:file "util")
                 (:file "auto-flatten-seq")
                 (:file "combinators")
                 (:file "reduction")
                 (:file "failure")
                 (:file "gll")
                 (:file "cfg")
                 (:file "abnf")
                 (:file "transform")
                 (:file "core")))))
