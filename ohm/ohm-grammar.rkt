#lang racket/base
;; The grammar for Ohm grammars themselves.

(provide proto-built-in-rules
         built-in-rules
         ohm-grammar-grammar
         system-grammars)

(require json)
(require racket/file)
(require "json-load.rkt")

(require racket/runtime-path)
(define-runtime-path ohm ".")

(define (load-system-grammar json-filename)
  (jsexpr->grammar (with-input-from-file (build-path ohm json-filename) read-json)))

(define proto-built-in-rules (load-system-grammar "proto-built-in-rules.json"))
(define built-in-rules (load-system-grammar "built-in-rules.json"))
(define ohm-grammar-grammar (load-system-grammar "ohm-grammar.json"))

(define system-grammars
  (hash 'ProtoBuiltInRules proto-built-in-rules
        'BuiltInRules built-in-rules
        'Ohm ohm-grammar-grammar))

(module+ test
  (require racket/pretty)
  (pretty-print system-grammars))
