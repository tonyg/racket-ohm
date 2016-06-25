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

         pexpr-arity
         pexpr-subst)

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

(define (pexpr-subst env expr)
  (let walk ((expr expr))
    (match expr
      [(pexpr-param index) (vector-ref env index)]
      [(pexpr-alt terms) (pexpr-alt (map walk terms))]
      [(pexpr-seq factors) (pexpr-seq (map walk factors))]
      [(pexpr-star inner-expr) (pexpr-star (walk inner-expr))]
      [(pexpr-plus inner-expr) (pexpr-plus (walk inner-expr))]
      [(pexpr-opt inner-expr) (pexpr-opt (walk inner-expr))]
      [(pexpr-not inner-expr) (pexpr-not (walk inner-expr))]
      [(pexpr-lookahead inner-expr) (pexpr-lookahead (walk inner-expr))]
      [(pexpr-lex inner-expr) (pexpr-lex (walk inner-expr))]
      [(pexpr-apply rule-name arguments) (pexpr-apply rule-name (map walk arguments))]
      [_ expr])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  (check-equal? (pexpr-arity (pexpr-seq (list (pexpr-not (pexpr-prim "\n"))
                                              (pexpr-apply 'space '()))))
                1))
