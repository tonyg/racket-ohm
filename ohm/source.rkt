#lang racket/base

(provide (struct-out position)
         input-source?
         port->input-source
         input-source-at-eof?
         input-source-value
         input-source-position
         input-source-next
         input-source-take
         input-source-drop)

(require racket/stream)
(require parser-tools/lex)

(struct input-source ([value* #:mutable]
                      [position* #:mutable]
                      [next* #:mutable])
  #:methods gen:stream
  [(define (stream-empty? s)
     (input-source-at-eof? s))
   (define (stream-first s)
     (input-source-value s))
   (define (stream-rest s)
     (input-source-next s))])

(define (port->input-source p)
  (port-count-lines! p)
  (input-source #f #f p))

(define (input-source-at-eof? s)
  (eof-object? (input-source-value s)))

(define (input-source-value s)
  (input-source-value* (force-input-source! s)))

(define (input-source-position s)
  (input-source-position* (force-input-source! s)))

(define (input-source-next s)
  (input-source-next* (force-input-source! s)))

(define (input-source-take s n)
  (list->string (for/list [(i (in-range n)) (c s) #:when (char? c)] c)))

(define (input-source-drop s n)
  (if (zero? n)
      s
      (input-source-drop (input-source-next s) (- n 1))))

(define (force-input-source! s)
  (unless (input-source-position* s)
    (define port (input-source-next* s))
    (define-values (line col offset) (port-next-location port))
    (define pos (position offset line col))
    (define val (read-char port))
    (set-input-source-value*! s val)
    (set-input-source-position*! s pos)
    (set-input-source-next*! s (if (eof-object? val)
                                   s
                                   (port->input-source port))))
  s)

(module+ test
  (require rackunit)
  (let ((i (port->input-source (open-input-string "hello"))))
    (check-equal? (for/list [(c i)] c)
                  (string->list "hello")))

  (let ((i (port->input-source (open-input-string "hello"))))
    (check-false (stream-empty? i))
    (check-false (input-source-at-eof? i))
    (check-equal? (input-source-value i) #\h)
    (check-equal? (input-source-value i) #\h) ;; check idempotent

    (check-equal? (input-source-position i) (position 1 1 0))
    (check-equal? (position-offset (input-source-position i)) 1)
    (check-equal? (position-line (input-source-position i)) 1)
    (check-equal? (position-col (input-source-position i)) 0)

    (check-equal? (input-source-value (input-source-next i)) #\e)
    (check-equal? (input-source-value (input-source-next i)) #\e) ;; check idempotent
    (check-equal? (input-source-position (input-source-next i)) (position 2 1 1))
    (check-false (eq? i (input-source-next i)))

    (check-equal? (input-source-value i) #\h) ;; not altered by examining further downstream
    (check-equal? (input-source-position i) (position 1 1 0))

    (check-equal? (input-source-take i 0) "")
    (check-equal? (input-source-take i 3) "hel")
    (check-equal? (input-source-take i 5) "hello")
    (check-equal? (input-source-take i 50) "hello"))

  (let ((i (port->input-source (open-input-string ""))))
    (check-true (stream-empty? i))
    (check-true (input-source-at-eof? i))
    (check-equal? (input-source-value i) eof)
    (check-equal? (input-source-position i) (position 1 1 0))
    (check-equal? (input-source-value (input-source-next i)) eof)
    (check-equal? (input-source-position (input-source-next i)) (position 1 1 0))

    (check-true (eq? i (input-source-next i))) ;; check that eof reuses sources
    ))
