#lang racket/base
;; Loading of Ohm grammars from externalised JSON values.

(provide jsexpr->grammar
         jsexpr->rule
         jsexpr->pexpr)

(require racket/match)
(require "grammar.rkt")
(require "pexpr.rkt")

(define (maybe-symbol s)
  (if (eq? s 'null)
      #f
      (string->symbol s)))

(define (jsexpr->grammar j)
  (match j
    [(list "grammar"
           _meta-info
           name
           super-grammar-name
           start-rule
           rules)
     (define parsed-rules (for/list [((name j) (in-hash rules))] (jsexpr->rule name j)))
     (define (rules-of-kind k) (for/list [(e parsed-rules) #:when (equal? (car e) k)] (cadr e)))
     (ohm-grammar (string->symbol name)
                  (maybe-symbol super-grammar-name)
                  (maybe-symbol start-rule)
                  (rules-of-kind "define")
                  (rules-of-kind "extend")
                  (rules-of-kind "override"))]))

(define (jsexpr->rule name j)
  (match j
    [(list kind _meta-info _description formals rule-body)
     (list kind
           (ohm-rule name
                     (map string->symbol formals)
                     (jsexpr->pexpr rule-body)))]))

(define (jsexpr->pexpr j)
  (let walk ((j j))
    (match j
      [(list "any" _meta-info) (pexpr-any)]
      [(list "end" _meta-info) (pexpr-end)]
      [(list "terminal" _meta-info obj) (pexpr-prim obj)]
      [(list "range" _meta-info from to) (pexpr-range from to)]
      [(list "param" _meta-info index) (pexpr-param index)]
      [(list* "alt" _meta-info terms) (pexpr-alt (map walk terms))]
      [(list* "seq" _meta-info factors) (pexpr-seq (map walk factors))]
      [(list "star" _meta-info expr) (pexpr-star (walk expr))]
      [(list "plus" _meta-info expr) (pexpr-plus (walk expr))]
      [(list "opt" _meta-info expr) (pexpr-opt (walk expr))]
      [(list "not" _meta-info expr) (pexpr-not (walk expr))]
      [(list "lookahead" _meta-info expr) (pexpr-lookahead (walk expr))]
      [(list "lex" _meta-info expr) (pexpr-lex (walk expr))]
      [(list "app" _meta-info rule-name args)
       (pexpr-apply (string->symbol rule-name) (map walk args))]
      [(list "unicodeChar" _meta-info category) (pexpr-unicode-char category)])))
