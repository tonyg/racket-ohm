#lang racket/base
;; Loading of Ohm grammars from externalised JSON values.

(provide jsexpr->grammar
         jsexpr->rule
         jsexpr->pexpr)

(require racket/match)
(require "grammar.rkt")
(require "pexpr.rkt")

(define (maybe f s)
  (if (eq? s 'null)
      #f
      (f s)))

(define (jsexpr->grammar j)
  (match j
    [(list "grammar"
           _meta-info
           name
           super-grammar-name
           start-rule
           rules)
     (define parsed-rules (for/list [((name j) (in-hash rules))] (jsexpr->rule name j)))
     (define (rules-of-kind k) (for/hash [(e parsed-rules) #:when (equal? (car e) k)]
                                 (define r (cadr e))
                                 (values (ohm-rule-name r) r)))
     (ohm-grammar (string->symbol name)
                  (maybe string->symbol super-grammar-name)
                  (maybe string->symbol start-rule)
                  (rules-of-kind "define")
                  (rules-of-kind "extend")
                  (rules-of-kind "override"))]))

(define (jsexpr->rule name j)
  (match j
    [(list kind _meta-info description formals rule-body)
     (list kind
           (ohm-rule name
                     (maybe values description)
                     (map string->symbol formals)
                     (jsexpr->pexpr rule-body)))]))

(define (jsexpr->pexpr j)
  (let walk ((j j))
    (match j
      [(list "any" _meta-info) (pexpr-any)]
      [(list "end" _meta-info) (pexpr-end)]
      [(list "terminal" _meta-info (? string? obj)) (pexpr-prim obj)]
      [(list "range" _meta-info from to) (pexpr-range from to)]
      [(list "param" _meta-info index) (pexpr-param index)]
      [(list* "alt" _meta-info terms) (apply-if-many pexpr-alt (map walk terms))]
      [(list* "seq" _meta-info factors) (apply-if-many pexpr-seq (map walk factors))]
      [(list "star" _meta-info expr) (pexpr-star (walk expr))]
      [(list "plus" _meta-info expr) (pexpr-plus (walk expr))]
      [(list "opt" _meta-info expr) (pexpr-opt (walk expr))]
      [(list "not" _meta-info expr) (pexpr-not (walk expr))]
      [(list "lookahead" _meta-info expr) (pexpr-lookahead (walk expr))]
      [(list "lex" _meta-info expr) (pexpr-lex (walk expr))]
      [(list "app" _meta-info rule-name args)
       (pexpr-apply (string->symbol rule-name) (map walk args))]
      [(list "unicodeChar" _meta-info category) (pexpr-unicode-char category)])))
