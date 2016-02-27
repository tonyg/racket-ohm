#lang racket/base
;; Loading of Ohm grammars from externalised JSON values.

(provide jsexpr->grammar
         jsexpr->rule
         jsexpr->grammar-hierarchy
         jsexpr->pexpr)

(require racket/match)
(require "grammar.rkt")
(require "pexpr.rkt")

(define (hash-map-values h f)
  (for/hash [((k v) (in-hash h))]
    (values k (f k v))))

(define (maybe-symbol s)
  (and s (string->symbol s)))

(define (jsexpr->grammar j)
  (match j
    [(hash-table ['name name]
                 ['definitions definitions]
                 ['extensions extensions]
                 ['overrides overrides]
                 _ ...)
     (define super-grammar-name (hash-ref j 'superGrammar #f))
     (define default-start-rule (hash-ref j 'defaultStartRule #f))
     (ohm-grammar (string->symbol name)
                  (maybe-symbol super-grammar-name)
                  (maybe-symbol default-start-rule)
                  (hash-map-values definitions (lambda (n j) (jsexpr->rule j)))
                  (hash-map-values extensions (lambda (n j) (jsexpr->rule j)))
                  (hash-map-values overrides (lambda (n j) (jsexpr->rule j))))]))

(define (jsexpr->rule j)
  (match j
    [(hash-table ['name name] ['formals formals] ['body body] _ ...)
     (ohm-rule (string->symbol name)
               (map string->symbol formals)
               (jsexpr->pexpr body))]))

(define (jsexpr->grammar-hierarchy j)
  (match j
    [(hash-table ['startGrammar start-grammar] ['grammars grammars] _ ...)
     (values (string->symbol start-grammar)
             (hash-map-values grammars (lambda (n j) (jsexpr->grammar j))))]))

(define (jsexpr->pexpr j)
  (let walk ((j j))
    (match j
      [(hash-table ['type "any"] _ ...) (pexpr-any)]
      [(hash-table ['type "end"] _ ...) (pexpr-end)]
      [(hash-table ['type "prim"] ['obj obj] _ ...) (pexpr-prim obj)]
      [(hash-table ['type "range"] ['from from] ['to to] _ ...) (pexpr-range from to)]
      [(hash-table ['type "param"] ['index index] _ ...) (pexpr-param index)]
      [(hash-table ['type "alt"] ['terms terms] _ ...) (pexpr-alt (map walk terms))]
      [(hash-table ['type "seq"] ['factors factors] _ ...) (pexpr-seq (map walk factors))]
      [(hash-table ['type "star"] ['expr expr] _ ...) (pexpr-star (walk expr))]
      [(hash-table ['type "plus"] ['expr expr] _ ...) (pexpr-plus (walk expr))]
      [(hash-table ['type "opt"] ['expr expr] _ ...) (pexpr-opt (walk expr))]
      [(hash-table ['type "not"] ['expr expr] _ ...) (pexpr-not (walk expr))]
      [(hash-table ['type "lookahead"] ['expr expr] _ ...) (pexpr-lookahead (walk expr))]
      [(hash-table ['type "lex"] ['expr expr] _ ...) (pexpr-lex (walk expr))]
      [(hash-table ['type "val"] ['expr expr] _ ...) (pexpr-val (walk expr))]
      [(hash-table ['type "arr"] ['expr expr] _ ...) (pexpr-val (walk expr))]
      [(hash-table ['type "val"] ['expr expr] _ ...) (pexpr-arr (walk expr))]
      [(hash-table ['type "obj"] ['lenient lenient?] ['properties properties] _ ...)
       (pexpr-obj lenient? (hash-map-values properties walk))]
      [(hash-table ['type "app"] ['rule rule-name] _ ...)
       (pexpr-apply (string->symbol rule-name)
                    (map walk (hash-ref j 'args '())))]
      [(hash-table ['type "unicodeChar"] ['category category] _ ...)
       (pexpr-unicode-char category)]
      [(hash-table ['type "typeCheck"] ['expectedType expected-type] _ ...)
       (pexpr-type-check (string->symbol expected-type))])))
