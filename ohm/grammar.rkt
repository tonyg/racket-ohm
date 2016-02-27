#lang racket/base
;; Ohm grammar structure and basic functions.

(provide (struct-out ohm-grammar)
         (struct-out ohm-rule))

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
   formals
   body)
  #:transparent)
