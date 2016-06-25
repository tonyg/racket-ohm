#lang racket/base
;; Very simple stretchy vector

(provide (rename-out [make-vec vec])
         vec?
         vec-length
         vec-default-value
         vec-ref
         vec-set!
         vec-add!
         vec->list
         vec->vector)

(require racket/match)

(struct vec ([length #:mutable]
             [height #:mutable]
             [root #:mutable]
             default-value
             fanout))
;; TODO: add printing? maybe make it optionally-opaque? (some of these
;; will be very large - unprintably large, perhaps)

(define nothing (cons 'nothing '())) ;; sentinel

(define (make-vec [initial-length 0] [default-value #f] #:fanout [fanout 128])
  (vec initial-length
       1
       (make-vector fanout default-value)
       default-value
       fanout))

(define (index->path height fanout n)
  (let walk ((acc '()) (height height) (n n))
    (if (zero? height)
        (if (zero? n)
            acc
            #f)
        (let ((upper-bits (quotient n fanout))
              (lower-bits (modulo n fanout)))
          (walk (cons lower-bits acc) (- height 1) upper-bits)))))

(define (oob who)
  (error who "Out-of-bounds index"))

(define (adjust-index who check-bounds? n len)
  (when (< n 0)
    (set! n (+ n len))
    (when (< n 0) (oob who)))
  (when (and check-bounds? (>= n len)) (oob who))
  n)

(define (vec-ref v n #:check-bounds? [check-bounds? #t])
  (match-define (vec len height root default-value fanout) v)
  (set! n (adjust-index 'vec-ref check-bounds? n len))
  (define path (index->path height fanout n))
  (if (not path)
      default-value
      (let loop ((piece root) (path path))
        (cond
          [(eq? piece nothing) default-value]
          [(null? path) piece]
          [else (loop (vector-ref piece (car path)) (cdr path))]))))

(define (grow-to-fit! v n)
  (define fanout (vec-fanout v))
  (when (>= n (expt fanout (vec-height v)))
    (set-vec-height! v (+ (vec-height v) 1))
    (define new-root (make-vector fanout nothing))
    (vector-set! new-root 0 (vec-root v))
    (set-vec-root! v new-root)
    (grow-to-fit! v n)))

(define (vec-set! v n val #:check-bounds? [check-bounds? #t])
  (set! n (adjust-index 'vec-set! check-bounds? n (vec-length v)))
  (grow-to-fit! v n)
  (match-define (vec len height root default-value fanout) v)
  (define path (index->path height fanout n))
  (when (not path) (error 'vec-set! "Internal error; path post-grow was #f"))
  (let loop ((piece root) (path path))
    (match path
      [(list i) (vector-set! piece i val)]
      [(cons i rest)
       (define next-piece (vector-ref piece i))
       (when (eq? next-piece nothing)
         (set! next-piece (make-vector fanout
                                       (if (null? (cdr rest))
                                           default-value
                                           nothing)))
         (vector-set! piece i next-piece))
       (loop next-piece rest)]))
  (when (>= n len) (set-vec-length! v (+ n 1)))
  v)

(define (vec-add! v val)
  (vec-set! v (vec-length v) val #:check-bounds? #f))

(define (vec->list v) (vector->list (vec->vector v)))

(define (vec->vector v)
  (match-define (vec len height root default-value fanout) v)
  (define result (make-vector len default-value))
  (let loop ((height height) (piece root) (base 0))
    (unless (eq? piece nothing)
      (define next-base (* base fanout))
      (if (= height 1)
          (vector-copy! result next-base piece 0 (min fanout (- len next-base)))
          (for ((i (in-naturals)) (next-piece (in-vector piece)))
            (loop (- height 1) next-piece (+ next-base i))))))
  result)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  (let ((v (make-vec 1 'X #:fanout 2)))
    (check-equal? (vec-length v) 1)
    (check-equal? (vec-ref v 0) 'X)
    (check-equal? (vec-ref v -1) 'X)
    (check-equal? (vec-ref v 1 #:check-bounds? #f) 'X)
    (vec-set! v 0 0)
    (check-equal? (vec-length v) 1)
    (check-equal? (vec-ref v 0) 0)
    (check-equal? (vec-ref v -1) 0)
    (check-equal? (vec-ref v 1 #:check-bounds? #f) 'X)
    (vec-set! v 10 10 #:check-bounds? #f)
    (check-equal? (vec-length v) 11)
    (check-equal? (vec-ref v 0) 0)
    (check-equal? (vec-ref v -1) 10)
    (check-equal? (vec-ref v 1) 'X)
    (check-equal? (vec-ref v 9) 'X)
    (check-equal? (vec-ref v 10) 10)
    (check-equal? (vec-ref v 11 #:check-bounds? #f) 'X)
    (check-equal? (vec->vector v) '#(0 X X X X X X X X X 10))))
