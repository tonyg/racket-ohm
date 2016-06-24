#lang racket/base
;; Ohm grammar structure and basic functions.

(provide (struct-out ohm-grammar)
         (struct-out ohm-rule)
         rule-name-syntactic?
         rule-name-lexical?)

(struct ohm-grammar
  (name
   super-grammar-name
   default-start-rule
   definitions
   extensions
   overrides)
  #:transparent)

(struct ohm-rule
  (name
   description
   formals
   body)
  #:transparent)

;; Rules are either "syntactic", meaning they deliberately discard
;; whitespace within embedded sequences, or "lexical", meaning
;; whitespace must be treated explicitly by the rule.
;;
;; Ohm's rule names start with capital letters exactly when they are
;; "syntactic".
(define (rule-name-syntactic? n)
  (define s (symbol->string n))
  (and (positive? (string-length s))
       (char-upper-case? (string-ref s 0))))

(define (rule-name-lexical? n)
  (not (rule-name-syntactic? n)))

(module+ test
  (require rackunit)
  (check-true (rule-name-syntactic? 'SyntacticRule))
  (check-false (rule-name-syntactic? 'lexicalRule))
  (check-false (rule-name-lexical? 'SyntacticRule))
  (check-true (rule-name-lexical? 'lexicalRule)))
