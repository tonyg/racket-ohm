#lang racket/base
;; The grammar for Ohm grammars themselves.

(provide load-ohm-grammar)

(require json)
(require racket/file)
(require "json-load.rkt")

(require racket/runtime-path)
(define-runtime-path ohm ".")

(define (load-ohm-grammar)
  (define ohm-grammar-json
    (with-input-from-file (build-path ohm "ohm-grammar.json") read-json))
  (jsexpr->grammar-hierarchy ohm-grammar-json))
