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

         (struct-out pexpr-unicode-char)

         pexpr-arity)

(require racket/match)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (pexpr-arity expr)
  (match expr
    [(? pexpr-any?) 1]
    [(? pexpr-end?) 1]
    [(? pexpr-prim?) 1]
    [(? pexpr-range?) 1]
    [(? pexpr-param?) 1] ;; TODO: bogus?!?!
    [(pexpr-alt terms) (if (null? terms) 0 (pexpr-arity (car terms)))]
    [(pexpr-seq factors) (foldl + 0 (map pexpr-arity factors))]
    [(pexpr-star inner-expr) (pexpr-arity inner-expr)]
    [(pexpr-plus inner-expr) (pexpr-arity inner-expr)]
    [(pexpr-opt inner-expr) (pexpr-arity inner-expr)]
    [(pexpr-not inner-expr) 0]
    [(pexpr-lookahead inner-expr) (pexpr-arity inner-expr)]
    [(pexpr-lex inner-expr) (pexpr-arity inner-expr)]
    [(? pexpr-apply?) 1]
    [(? pexpr-unicode-char?) 1]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  (check-equal? (pexpr-arity (pexpr-seq (list (pexpr-not (pexpr-prim "\n"))
                                              (pexpr-apply 'space '()))))
                1))
