#lang racket/base

(provide read-ohm-grammars
         read-ohm-grammar
         parse-ohm-grammar)

(require racket/match)
(require (only-in racket/list flatten))

(require "match.rkt")
(require "grammar.rkt")
(require "pexpr.rkt")
(require "ohm-grammar.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-ohm-grammar [p (current-input-port)]
                          #:start-rule [start-rule 'Grammar]
                          #:input-source-name [input-source-name #f])
  (read/grammar ohm-grammar-grammar
                p
                #:input-source-name input-source-name
                #:start-rule start-rule
                #:on-success (lambda (r is) (parse-ohm-grammar r))))

(define (read-ohm-grammars [p (current-input-port)]
                           #:input-source-name [input-source-name #f])
  (read-ohm-grammar p
                    #:start-rule 'Grammars
                    #:input-source-name input-source-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct rule-def (kind rule) #:prefab)

(define parse-ohm-grammar
  (ohm-semantics
   ()

   [(Grammars gs)
    (for/hash [(g (parse-ohm-grammar gs))]
      (values (ohm-grammar-name g) g))]

   [(Grammar (#:text name) sg _ rules _)
    (define parsed-rules (flatten (parse-rule rules #f '() #f)))
    (define (rules-of-kind k) (for/hash [(e parsed-rules) #:when (equal? (rule-def-kind e) k)]
                                (define r (rule-def-rule e))
                                (values (ohm-rule-name r) r)))
    (ohm-grammar (string->symbol name)
                 (match (parse-ohm-grammar sg)
                   [(list sgname) (string->symbol sgname)]
                   [(list) #f])
                 (and (pair? parsed-rules) (ohm-rule-name (rule-def-rule (car parsed-rules))))
                 (rules-of-kind 'Rule_define)
                 (rules-of-kind 'Rule_extend)
                 (rules-of-kind 'Rule_override))]

   [(SuperGrammar _ (#:text sgname))
    sgname]))

(define parse-formals
  (ohm-semantics
   ()
   [(Formals _ idents _) (parse-formals idents)]
   [(NonemptyListOf e0 _ (#:seq es)) (parse-formals (cons e0 es))]
   [(EmptyListOf) '()]
   [(ident (#:text name)) (string->symbol name)]))

(define (build-rule name-str rule-kind maybe-formals maybe-description body)
  (define rule-name (string->symbol name-str))
  (define formals (parse-formals maybe-formals))
  (define parsed-body (flatten (parse-rule body rule-name formals rule-kind)))
  (cons
   (rule-def rule-kind
             (ohm-rule rule-name
                       (match (ohm-match maybe-description [(ruleDescr _ (#:text d) _) d])
                         [(list d) d]
                         [(list) #f])
                       formals
                       (match (filter pexpr? parsed-body) [(list x) x])))
   (filter rule-def? parsed-body)))

(define parse-rule
  (ohm-semantics
   (rule-name formals rule-kind)

   [(Rule_define (#:text name) maybe-formals maybe-description _ body)
    (build-rule name 'Rule_define maybe-formals maybe-description body)]

   [(Rule_override (#:text name) maybe-formals _ body)
    (build-rule name 'Rule_override maybe-formals '() body)]

   [(Rule_extend (#:text name) maybe-formals _ body)
    (build-rule name 'Rule_extend maybe-formals '() body)]

   [(RuleBody _ (NonemptyListOf term _ (#:seq terms)))
    (define parsed-terms (flatten (parse-rule (cons term terms) rule-name formals rule-kind)))
    (cons (apply-if-many pexpr-alt (filter pexpr? parsed-terms))
          (filter rule-def? parsed-terms))]

   [(TopLevelTerm c)
    (if (eq? (ohm-nonterminal-rule-name c) 'Seq)
        (parse-expr c)
        (parse-rule c rule-name formals rule-kind))]

   [(TopLevelTerm_inline seq case-name-node)
    (define case-name (ohm-match case-name-node [(caseName _ _ (#:text n) _ _) n]))
    (define inline-name (string->symbol (format "~a_~a" rule-name case-name)))
    (list (pexpr-apply inline-name formals)
          (rule-def rule-kind
                    (ohm-rule inline-name
                              #f
                              formals
                              (parse-expr seq))))]))

(define parse-expr
  (ohm-semantics
   ()
   [(Alt (NonemptyListOf s0 _ (#:seq ss))) (apply-if-many pexpr-alt (parse-expr (cons s0 ss)))]
   [(Seq es) (apply-if-many pexpr-seq (parse-expr es))]
   [(Iter_star p _) (pexpr-star (parse-expr p))]
   [(Iter_plus p _) (pexpr-plus (parse-expr p))]
   [(Iter_opt p _) (pexpr-opt (parse-expr p))]
   [(Pred_not _ p) (pexpr-not (parse-expr p))]
   [(Pred_lookahead _ p) (pexpr-lookahead (parse-expr p))]
   [(Lex_lex _ e) (pexpr-lex (parse-expr e))]
   [(Base_application (#:text rule-name-str) (#:seq params))
    (pexpr-apply (string->symbol rule-name-str)
                 (if (null? params) '() (parse-expr (car params))))]
   [(Base_range from _ to) (pexpr-range (parse-expr from) (parse-expr to))]
   [(Base_terminal t) (pexpr-prim (parse-expr t))]
   [(Base_paren _ a _) (parse-expr a)]
   [(Params _ ps _) (parse-expr ps)]
   [(NonemptyListOf e0 _ (#:seq es)) (parse-expr (cons e0 es))]
   [(EmptyListOf) '()]
   [(oneCharTerminal _ (#:text escaped) _) (unescape-string escaped)]
   [(terminal _ (#:text escaped) _) (unescape-string escaped)]))

(define (unescape-string s)
  (list->string
   (let loop ((cs (string->list s)))
     (match cs
       ['() '()]
       [(list* #\\ #\\ rest) (cons #\\ (loop rest))]
       [(list* #\\ #\b rest) (cons #\backspace (loop rest))]
       [(list* #\\ #\n rest) (cons #\newline (loop rest))]
       [(list* #\\ #\r rest) (cons #\return (loop rest))]
       [(list* #\\ #\t rest) (cons #\tab (loop rest))]
       [(list* #\\ #\u x1 x2 x3 x4 rest) (cons (integer->char
                                                (string->number (string x1 x2 x3 x4) 16))
                                               (loop rest))]
       [(list* #\\ #\x x1 x2 rest) (cons (integer->char (string->number (string x1 x2) 16))
                                         (loop rest))]
       [(list* #\\ x rest) (cons x (loop rest))]
       [(list* x rest) (cons x (loop rest))]))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require racket/port)
  (require racket/pretty)
  (require racket/set)
  (require rackunit)

  (require racket/runtime-path)
  (define-runtime-path ohm ".")
  (define input-filename (build-path ohm "private/ohm-grammar.ohm"))
  (define v (call-with-input-file
              input-filename
              (lambda (p)
                (read/grammar #:input-source-name input-filename
                              ohm-grammar-grammar p))))
  (define new-ohm-grammar-grammar (hash-ref (parse-ohm-grammar v) 'Ohm))
  ;; (pretty-print new-ohm-grammar-grammar)

  (define (check-rules old-rules new-rules)
    (define missing-from-old (set-subtract (hash-keys new-rules) (hash-keys old-rules)))
    (define missing-from-new (set-subtract (hash-keys old-rules) (hash-keys new-rules)))
    (check-equal? '() missing-from-old)
    (check-equal? '() missing-from-new)
    (for [((name old-rule) (in-hash old-rules))]
      (define new-rule (hash-ref new-rules name))
      (check-equal? new-rule old-rule)))

  (match* (ohm-grammar-grammar new-ohm-grammar-grammar)
    [((ohm-grammar old-name old-super old-start old-defs old-exts old-overs)
      (ohm-grammar new-name new-super new-start new-defs new-exts new-overs))
     (check-equal? new-name old-name)
     (check-equal? new-super old-super)
     (check-equal? new-start old-start)
     (check-rules old-defs new-defs)
     (check-rules old-exts new-exts)
     (check-rules old-overs new-overs)])

  (check-equal? new-ohm-grammar-grammar ohm-grammar-grammar)

  (check-exn #px"Parse error at :1\\.13 .* \":=\" in order to match rule Rule_override" ;; etc etc
             (lambda ()
               (read-ohm-grammar (open-input-string "Bogus { quux }"))))

  ;; (read-ohm-grammar (open-input-string "Bogus { "))

  )
