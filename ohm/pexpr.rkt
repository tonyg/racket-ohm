#lang racket/base
;; Parsing Expressions.
;; Data structure definitions for AST nodes in Ohm grammars.

(provide (struct-out pexpr)

         (struct-out pexpr-any)
         (struct-out pexpr-end)

         (struct-out pexpr-prim)
         (struct-out pexpr-range)
         (struct-out pexpr-param)
         (struct-out pexpr-alt)
         (struct-out pexpr-seq)

         (struct-out pexpr-iter)
         (struct-out pexpr-star)
         (struct-out pexpr-plus)
         (struct-out pexpr-opt)

         (struct-out pexpr-not)
         (struct-out pexpr-lookahead)

         (struct-out pexpr-lex)

         (struct-out pexpr-apply)

         (struct-out pexpr-unicode-char))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct pexpr () #:transparent)

(struct pexpr-any pexpr () #:transparent)
(struct pexpr-end pexpr () #:transparent)

(struct pexpr-prim pexpr (obj) #:transparent)
(struct pexpr-range pexpr (from to) #:transparent)
(struct pexpr-param pexpr (index) #:transparent)
(struct pexpr-alt pexpr (terms) #:transparent)
(struct pexpr-seq pexpr (factors) #:transparent)

(struct pexpr-iter pexpr (expr) #:transparent)
(struct pexpr-star pexpr-iter () #:transparent)
(struct pexpr-plus pexpr-iter () #:transparent)
(struct pexpr-opt pexpr-iter () #:transparent)

(struct pexpr-not pexpr (expr) #:transparent)
(struct pexpr-lookahead pexpr (expr) #:transparent)

(struct pexpr-lex pexpr (expr) #:transparent)

(struct pexpr-apply pexpr (rule-name arguments) #:transparent)

(struct pexpr-unicode-char pexpr (category) #:transparent)
