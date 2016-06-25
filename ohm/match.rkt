#lang racket/base

;; TODO: left recursion

(provide (struct-out meta-info)
         (struct-out ohm-node)
         (struct-out ohm-terminal)
         (struct-out ohm-nonterminal)
         (struct-out exn:fail:read:ohm)
         (struct-out failing-pexpr)
         ohm-node->s-expression
         read/grammar
         ohm-match
         ohm-semantics)

(require racket/match)
(require racket/set)
(require (only-in racket/list make-list group-by))
(require syntax/srcloc)
(require (only-in racket/string string-join))

(require (for-syntax racket/base))

(require "grammar.rkt")
(require "ohm-grammar.rkt")
(require "pexpr.rkt")
(require "source.rkt")
(require "unicode-categories.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct meta-info (source-name line col offset span source) #:prefab)
(struct ohm-node (meta-info) #:prefab)
(struct ohm-sequence ohm-node (items) #:prefab)
(struct ohm-terminal ohm-node (value) #:prefab)
(struct ohm-nonterminal ohm-node (rule-name children) #:prefab)

(struct exn:fail:read:ohm exn:fail:read (failing-pexprs) #:transparent)
(struct failing-pexpr (rule-name expr) #:transparent)

(struct pos-info (pos [cache #:mutable]) #:transparent)

(struct environment (inheritance-chain bindings) #:transparent)

(define (ohm-node->s-expression n)
  (match n
    [(ohm-sequence _ ns) (map ohm-node->s-expression ns)]
    [(ohm-terminal _ v) v]
    [(ohm-nonterminal _ r ns) (cons r (map ohm-node->s-expression ns))]))

(define current-input-source-name (make-parameter #f))
(define current-input-source (make-parameter #f))
(define current-rule-name (make-parameter #f))
(define current-rule-name-for-failures (make-parameter #f))
(define current-bindings (make-parameter #f))
(define in-lexified-context? (make-parameter #f))
(define current-memo-table (make-parameter #f))
(define current-rightmost-failure-position (make-parameter #f))
(define current-rightmost-failing-pexprs (make-parameter #f))

(define (pos-info-at pos)
  (hash-ref (current-memo-table)
            pos
            (lambda ()
              (define i (pos-info pos (hash)))
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
  (define span (- (position-offset p1) (position-offset p0)))
  (meta-info (current-input-source-name)
             (position-line p0)
             (position-col p0)
             (position-offset p0)
             span
             (input-source-take is0 span)))

(define (terminal is0 v #:end-pos [is1 (peek)])
  (ohm-terminal (interval is0 is1) v))

(define (nonterminal is0 rule-name children #:end-pos [is1 (peek)])
  (ohm-nonterminal (interval is0 is1) rule-name children))

(define (succeed n)
  (current-bindings (cons n (current-bindings)))
  #t)

(define (transpose-bindings is0 expr rows #:end-pos [is1 (peek)])
  (define mi (interval is0 is1))
  (if (null? rows)
      (make-list (pexpr-arity expr) (ohm-sequence mi '()))
      (apply map (lambda ns (ohm-sequence mi ns)) rows)))

(define (succeed/transpose is0 expr rows)
  (for [(v (transpose-bindings is0 expr rows))] (succeed v))
  #t)

(define (fail is expr)
  (define new-offset (position-offset (input-source-position is)))
  (define old-offset (position-offset (current-rightmost-failure-position)))
  (cond
    [(> new-offset old-offset)
     (current-rightmost-failure-position (input-source-position is))
     (current-rightmost-failing-pexprs
      (set (failing-pexpr (current-rule-name-for-failures) expr)))]
    [(= new-offset old-offset)
     (current-rightmost-failing-pexprs
      (set-add (current-rightmost-failing-pexprs)
               (failing-pexpr (current-rule-name-for-failures) expr)))]
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
                    (current-rightmost-failing-pexprs #f))
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
         (succeed (terminal is (input-source-value is))))]
    [(pexpr-end)
     (if (input-source-at-eof? is)
         (succeed (terminal is (void)))
         (fail is expr))]
    [(pexpr-prim obj)
     (if (equal? (input-source-take is (string-length obj)) obj)
         (begin (poke! (input-source-drop is (string-length obj)))
                (succeed (terminal is obj)))
         (fail is expr))]
    [(pexpr-range from to)
     (advance! is)
     (if (input-source-at-eof? is)
         (fail is expr)
         (let* ((ch (input-source-value is))
                (s (string ch)))
           (if (and (string<=? from s) (string<=? s to))
               (succeed (terminal is ch))
               (fail is expr))))]
    [(pexpr-param index)
     (eval-pexpr* is
                  (environment (environment-inheritance-chain env) '#())
                  (vector-ref (environment-bindings env) index))]
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
     (define actuals (for/vector [(e arguments)] (pexpr-subst (environment-bindings env) e)))
     (define application (list rule-name actuals))
     (define pi (pos-info-at (input-source-position is)))
     (match-define (cons children next-is)
       (hash-ref (pos-info-cache pi)
                 application
                 (lambda ()
                   (define entry
                     (cons (parameterize ((current-rule-name rule-name)
                                          (current-rule-name-for-failures
                                           (if (or (not (current-rule-name-for-failures))
                                                   (in-syntactic-context?))
                                               rule-name
                                               (current-rule-name-for-failures))))
                             (eval-pexpr/bindings (environment (environment-inheritance-chain env)
                                                               actuals)
                                                  (lookup-rule-body env rule-name)))
                           (peek)))
                   (set-pos-info-cache! pi (hash-set (pos-info-cache pi) application entry))
                   entry)))
     (and children
          (begin (poke! next-is)
                 (succeed (nonterminal is rule-name children))))]
    [(pexpr-unicode-char category)
     (define rx
       (hash-ref unicode-categories category
                 (lambda () (error 'eval-pexpr "Cannot find Unicode category ~v" category))))
     (advance! is)
     (if (input-source-at-eof? is)
         (fail is expr)
         (let ((ch (input-source-value is)))
           (if (regexp-match rx (string ch))
               (succeed (terminal is ch))
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

(define (pexpr->expected expr)
  (match expr
    [(pexpr-any) "any character"]
    [(pexpr-end) "end-of-file"]
    [(pexpr-prim obj) (format "the token ~v" obj)]
    [(pexpr-range from to) (format "anything between ~v and ~v" from to)]
    [(pexpr-param _) "something matching one of the parameters given to a parameterized rule"]
    [(pexpr-alt (list* p _)) (pexpr->expected p)]
    [(pexpr-seq (list* p _)) (pexpr->expected p)]
    [(pexpr-iter p) (pexpr->expected p)]
    [(pexpr-not p) (string-append "anything but " (pexpr->expected p))]
    [(pexpr-lookahead p) (pexpr->expected p)]
    [(pexpr-lex p) (pexpr->expected p)]
    [(pexpr-apply r _) (format "something matching rule ~a" r)]
    [(pexpr-unicode-char category) (format "a Unicode character in category ~v" category)]))

(define (failing-pexprs->string failing-pexprs-set)
  (define failing-pexprs (set->list failing-pexprs-set))
  (define grouped-by-rule-name (group-by failing-pexpr-rule-name failing-pexprs))
  (string-join
   (for/list [(entries grouped-by-rule-name)]
     (define rule-name (failing-pexpr-rule-name (car entries)))
     (if (null? (cdr entries))
         (format "   - ~a in order to match rule ~a"
                 (pexpr->expected (failing-pexpr-expr (car entries)))
                 rule-name)
         (format "   - in order to match rule ~a, any of\n~a"
                 rule-name
                 (string-join
                  (sort
                   (for/list [(fp entries)]
                     (format "       - ~a" (pexpr->expected (failing-pexpr-expr fp))))
                   string<?)
                  "\n"))))
   "\n"))

(define (build-exn:fail:read:ohm input-source-name
                                 grammar-name
                                 start-rule
                                 pos
                                 failing-pexprs)
  (define loc (srcloc input-source-name
                      (position-line pos)
                      (position-col pos)
                      (position-offset pos)
                      #f))
  (exn:fail:read:ohm
   (format "Parse error at ~a while trying to read ~a using grammar ~a:\nExpected one of:\n~a"
           (source-location->string loc)
           start-rule
           grammar-name
           (failing-pexprs->string failing-pexprs))
   (current-continuation-marks)
   (list loc)
   failing-pexprs))

(define (read/grammar g
                      [source0 (current-input-port)]
                      #:input-source-name [input-source-name #f]
                      #:start-rule [start-rule (ohm-grammar-default-start-rule g)]
                      #:grammars [grammars (hash)]
                      #:on-success [ks values]
                      #:on-failure [kf raise])
  (define source
    (cond
      [(port? source0) (port->input-source source0)]
      [(input-source? source0) source0]
      [else (error 'read/grammar "Expected port or input-source; got ~v" source0)]))
  (define r
    (parameterize ((current-input-source-name input-source-name)
                   (current-input-source (box source))
                   (in-lexified-context? #f)
                   (current-memo-table (hash)))
      (define env (environment (build-inheritance-chain g grammars) '#()))
      (with-restored-failure-info
        (or (eval-pexpr/bindings env (pexpr-apply start-rule '()))
            (build-exn:fail:read:ohm (current-input-source-name)
                                     (ohm-grammar-name g)
                                     start-rule
                                     (current-rightmost-failure-position)
                                     (current-rightmost-failing-pexprs))))))
  (if (exn:fail:read:ohm? r)
      (kf r)
      (ks (car r))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(begin-for-syntax
  (define (compile-pat stx)
    (syntax-case stx ()
      [(#:text id)
       #'(ohm-node (app meta-info-source id))]
      [(#:terminal val)
       #`(ohm-terminal _ val)]
      [(#:node rule-name children)
       #`(ohm-nonterminal _ rule-name children)]
      [(#:seq ns)
       #`(ohm-sequence _ ns)]
      [(rule-name #:meta id kid ...)
       #`(ohm-nonterminal id 'rule-name (list #,@(map compile-pat (syntax->list #'(kid ...)))))]
      [(rule-name kid ...)
       #`(ohm-nonterminal _ 'rule-name (list #,@(map compile-pat (syntax->list #'(kid ...)))))]
      [id
       (identifier? #'id)
       #'id])))

(define-syntax (ohm-match stx)
  (syntax-case stx ()
    [(_ n [node-pat body ...] ...)
     #`(let loop ((n n))
         (match n
           #,@(map (lambda (clause)
                     (syntax-case clause ()
                       [(node-pat body ...)
                        #`[#,(compile-pat #'node-pat) #:when (ohm-node? n) body ...]]))
                   (syntax->list #'((node-pat body ...) ...)))
           [(ohm-nonterminal _ _ (list inner)) (loop inner)]
           [(ohm-sequence _ ns) (map loop ns)]
           [(? list? ns) (map loop ns)]))]))

(define-syntax (ohm-semantics stx)
  (syntax-case stx ()
    [(_ (extra-argument ...) [node-pat body ...] ...)
     #`(lambda (n extra-argument ...)
         (ohm-match n [node-pat body ...] ...))]))
