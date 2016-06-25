#lang racket/base

(require racket/match)
(require racket/set)
(require (only-in racket/list make-list))

(require "grammar.rkt")
(require "ohm-grammar.rkt")
(require "pexpr.rkt")
(require "source.rkt")
(require "unicode-categories.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct pos-info (pos) #:transparent)

(struct environment (inheritance-chain bindings) #:transparent)
(struct closure (env expr) #:transparent)

(define current-input-source-name (make-parameter #f))
(define current-input-source (make-parameter #f))
(define current-rule-name (make-parameter #f))
(define current-bindings (make-parameter #f))
(define in-lexified-context? (make-parameter #f))
(define current-memo-table (make-parameter #f))
(define current-rightmost-failure-position (make-parameter #f))
(define current-rightmost-failure-exprs (make-parameter #f))

(define (pos-info-at pos)
  (hash-ref (current-memo-table)
            pos
            (lambda ()
              (define i (pos-info pos))
              (current-memo-table (hash-set (current-memo-table) pos i))
              i)))

(define (in-syntactic-context?)
  (define rule-name (current-rule-name))
  (and rule-name
       (rule-name-syntactic? rule-name)
       (not (in-lexified-context?))))

(define (simple-rule-lookup h name)
  (define r (hash-ref h name #f))
  (and r (ohm-rule-body r)))

(define (lookup-rule-body env name)
  (let search ((ic (environment-inheritance-chain env)) (extending? #f))
    (match ic
      ['()
       (if extending?
           (error 'lookup-rule-body "Attempted to extend undefined rule ~a" name)
           (error 'lookup-rule-body "Undefined rule ~a" name))]
      [(cons (? ohm-grammar? g) rest)
       (or (simple-rule-lookup (ohm-grammar-overrides g) name)
           (simple-rule-lookup (ohm-grammar-definitions g) name)
           (let ((ext (simple-rule-lookup (ohm-grammar-extensions g) name)))
             (if ext
                 (pexpr-alt (list ext (search rest #t)))
                 (search rest extending?))))])))

(define (peek)
  (unbox (current-input-source)))

(define (poke! is)
  (set-box! (current-input-source) is))

(define (advance! is)
  (poke! (input-source-next is)))

(define (interval is0 [is1 (peek)])
  (define p0 (input-source-position is0))
  (define p1 (input-source-position is1))
  (vector (current-input-source)
          (position-line p0)
          (position-col p0)
          (position-offset p0)
          (- (position-offset p1) (position-offset p0))))

(define (succeed is0 v #:end-pos [is1 (peek)])
  (define srcloc (interval is0 is1))
  (current-bindings (cons (syntax-property (datum->syntax #f v srcloc)
                                           'ohm-source
                                           (input-source-take is0 (vector-ref srcloc 4)))
                          (current-bindings)))
  #t)

(define (transpose-bindings expr rows)
  (if (null? rows)
      (make-list (pexpr-arity expr) '())
      (apply map list rows)))

(define (succeed/transpose is expr rows)
  (for [(v (transpose-bindings expr rows))] (succeed is v))
  #t)

(define (fail is expr)
  (define new-offset (position-offset (input-source-position is)))
  (define old-offset (position-offset (current-rightmost-failure-position)))
  (cond
    [(> new-offset old-offset)
     (current-rightmost-failure-position (input-source-position is))
     (current-rightmost-failure-exprs (set expr))]
    [(= new-offset old-offset)
     (current-rightmost-failure-exprs (set-add (current-rightmost-failure-exprs) expr))]
    [else (void)])
  #f)

(define (eval-pexpr/bindings env expr)
  (parameterize ((current-bindings '()))
    (and (eval-pexpr env expr)
         (reverse (current-bindings)))))

(define (eval-pexpr-star env expr)
  (let loop ((rows '()))
    (define is (peek))
    (define bs (eval-pexpr/bindings env expr))
    (if bs
        (loop (cons bs rows))
        (begin0 (reverse rows)
          (poke! is)))))

(define-syntax with-restored-failure-info
  (syntax-rules ()
    [(_ body ...)
     (parameterize ((current-rightmost-failure-position (position -1 -1 0))
                    (current-rightmost-failure-exprs #f))
       body ...)]))

(define-syntax with-ignored-bindings
  (syntax-rules ()
    [(_ body ...)
     (parameterize ((current-bindings '()))
       body ...)]))

(define (eval-pexpr env expr)
  (define is0 (peek))
  (when (in-syntactic-context?)
    (with-restored-failure-info
      (with-ignored-bindings
        (eval-pexpr* is0 env (pexpr-apply 'spaces '())))))
  (define is1 (peek))
  (eval-pexpr* is1 env expr))

(define (eval-pexpr* is env expr)
  (match expr
    [(pexpr-any)
     (advance! is)
     (if (input-source-at-eof? is)
         (fail is expr)
         (succeed is (input-source-value is)))]
    [(pexpr-end)
     (if (input-source-at-eof? is)
         (succeed is (void))
         (fail is expr))]
    [(pexpr-prim obj)
     (if (equal? (input-source-take is (string-length obj)) obj)
         (begin (poke! (input-source-drop is (string-length obj)))
                (succeed is obj))
         (fail is expr))]
    [(pexpr-range from to)
     (advance! is)
     (if (input-source-at-eof? is)
         (fail is expr)
         (let* ((ch (input-source-value is))
                (s (string ch)))
           (if (and (string<=? from s) (string<=? s to))
               (succeed is ch)
               (fail is expr))))]
    [(pexpr-param index)
     (define c (vector-ref (environment-bindings env) index))
     (eval-pexpr (closure-env c) (closure-expr c))]
    [(pexpr-alt terms)
     (define saved-bindings (current-bindings))
     (let loop ((terms terms))
       (poke! is)
       (current-bindings saved-bindings)
       (match terms
         ['() (fail is expr)]
         [(cons term rest)
          (or (eval-pexpr env term) (loop rest))]))]
    [(pexpr-seq factors)
     (let loop ((factors factors))
       (match factors
         ['() #t]
         [(cons factor rest)
          (and (eval-pexpr env factor) (loop rest))]))]
    [(pexpr-star inner-expr)
     (succeed/transpose is expr (eval-pexpr-star env inner-expr))]
    [(pexpr-plus inner-expr)
     (define r (eval-pexpr/bindings env inner-expr))
     (and r (let ((rr (eval-pexpr-star env inner-expr)))
              (succeed/transpose is expr (cons r rr))))]
    [(pexpr-opt inner-expr)
     (define r (eval-pexpr/bindings env inner-expr))
     (if r
         (succeed/transpose is expr (list r))
         (begin (poke! is)
                (succeed/transpose is expr '())))]
    [(pexpr-not inner-expr)
     (if (with-restored-failure-info
           (with-ignored-bindings
             (eval-pexpr env inner-expr)))
         (fail is expr)
         #t)]
    [(pexpr-lookahead inner-expr)
     (begin0 (eval-pexpr env inner-expr)
       (poke! is))]
    [(pexpr-lex inner-expr)
     (parameterize ((in-lexified-context? #t))
       (eval-pexpr env inner-expr))]
    [(pexpr-apply rule-name arguments)
     (parameterize ((current-rule-name rule-name))
       (define r (eval-pexpr/bindings (environment (environment-inheritance-chain env)
                                                   (for/vector [(arg-expr arguments)]
                                                     (closure env arg-expr)))
                                      (lookup-rule-body env rule-name)))
       (and r (succeed is (cons (datum->syntax #f rule-name) r))))]
    [(pexpr-unicode-char category)
     (define rx
       (hash-ref unicode-categories category
                 (lambda () (error 'eval-pexpr "Cannot find Unicode category ~v" category))))
     (advance! is)
     (if (input-source-at-eof? is)
         (fail is expr)
         (let ((ch (input-source-value is)))
           (if (regexp-match rx (string ch))
               (succeed is ch)
               (fail is expr))))]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (super-grammar-not-found g0 super-grammar-name)
  (lambda ()
    (error 'build-inheritance-chain
           "Inheritance chain for grammar ~a refers to unavailable grammar ~a"
           (ohm-grammar-name g0)
           super-grammar-name)))

(define (build-inheritance-chain g0 env)
  (let loop ((g g0))
    (define super-grammar-name (ohm-grammar-super-grammar-name g))
    (cons g
          (if super-grammar-name
              (loop (hash-ref env
                              super-grammar-name
                              (super-grammar-not-found g0 super-grammar-name)))
              (list built-in-rules
                    proto-built-in-rules)))))

(define (grammar-match g
                       source0
                       #:input-source-name [input-source-name #f]
                       #:rule-name [rule-name (ohm-grammar-default-start-rule g)]
                       #:grammars [grammars (hash)])
  (define source
    (cond
      [(port? source0) (port->input-source source0)]
      [(input-source? source0) source0]
      [else (error 'grammar-match "Expected port or input-source; got ~v" source0)]))
  (define r
    (parameterize ((current-input-source-name input-source-name)
                   (current-input-source (box source))
                   (in-lexified-context? #f)
                   (current-memo-table (hash)))
      (define env (environment (build-inheritance-chain g grammars) '#()))
      (with-restored-failure-info
        (eval-pexpr/bindings env (pexpr-apply rule-name '())))))
  (and r (car r)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ main ;; TODO: main -> test
  (require racket/port)
  (require racket/pretty)
  (define v (call-with-input-file
              "es5.ohm"
              (lambda (p)
                (grammar-match ohm-grammar-grammar p))))
  (pretty-print (if (syntax? v)
                    (syntax->datum v)
                    v))
  (when (syntax? v)
    (syntax-case v (Grammars)
      [(Grammars (g ...))
       (for [(x (syntax->list #'(g ...)))]
         (write (syntax-property x 'ohm-source))
         (newline)
         (newline))])))
