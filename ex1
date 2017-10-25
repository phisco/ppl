#lang racket
(define (hello-world) (display "Hello world"))

(define (factorial x)
  (define (fact-tail x acc)
    (if (= 1 x) acc (fact-tail (- x 1) (* acc x))))
  (if (= 0 x) 1 (fact-tail x 1))
  )

(define (reverse-a-pair p)
  (if (not (pair? p))
      (error "pair plz")
  (let ((a (car p)) (b (cdr p)))
    (cons (car b) a)))
  )

(define (odd? x) (not (= 0 (modulo x 2))))

(define (pal x) (let ((l (string->list x)))
                  (equal? l (reverse l))))

(define (sum-of-square l)
  (define (sos-tail l acc) (if (null? l) acc
                               (sos-tail (cdr l) (+ acc (* (car l) (car l))))))
  (sos-tail l 0))

(define (range n m)
  (define (range-tail n l) (if (>= n m) (cons n l) (range-tail (+ 1 n) (cons n l)) ))
  (reverse (range-tail n '())))

(define (range-named-let lo hi)
  (let loop ((l lo)(acc '()))
    (if (>= l hi)
        acc (loop (+ l 1) (append acc (list l))))))

(define (flatten l)
  (define (flatten-tail l acc)
    (if (null? l) acc (let ((x (car l)) (xs (cdr l)))
                        (if (not (list? x)) (flatten-tail xs (cons x acc)) (flatten-tail xs (flatten-tail x acc))))))
  (reverse (flatten-tail l '())))

(define (quick-sort l)
  (if (null? l)
      l
  (let ((x (car l))
        (xs (cdr l)))
    (if (null? xs) (list x)
        (let* ((pivot x)
               (smaller (filter (lambda (x) (<= x pivot)) xs))
               (greater (filter (lambda (x) (> x pivot)) xs)))
          (append (qs smaller) (list x) (qs greater)))) )
    ))

(define (pack l)
  (define (pack-t l sub acc)
    (cond ((null? l)
           (cons sub acc) )
          ((or (null? sub) (equal? (car l) (car sub)))
           (pack-t (cdr l) (cons (car l) sub) acc))
          (else (pack-t (cdr l) (list (car l)) (cons sub acc)))
  ))
  (reverse (pack-t l '() '()))
  )

#lang racket


(define-syntax my-let* (syntax-rules ()
                         ((_ ((var val)) istr ...) 
                          ((λ (var)
                             istr ...)
                           val))
                         ((_ ((var val) . rest) istr ...)
                          ((λ (var)
                             (my-let* rest istr ...))
                           val))))

(define (factorial n)
  (if (= n 0) 1
      (* n (factorial (- n 1))))
  )

(define-syntax while
  (syntax-rules ()
               ((_ condition body ...)
                (let loop ()
                  (when condition (begin
                                    body ...
                                    (loop)))))))
