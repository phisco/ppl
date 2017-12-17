#lang racket

(define *queue* '())

(define (empty-queue?)
  (null? *queue*))

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x))

(define (fork proc)
  (call/cc (lambda(k)
             (enqueue k)
             (proc))))

(define (yield)
  (call/cc (lambda(k)
             (enqueue k)
             ((dequeue)))))

(define (c-exit)
  (if (empty-queue?)
      "End"
      ((dequeue))))

(define ((do-stuff-n-print str max))
  (let loop ((n 0))
    (display str)
    (display " ")
    (display n)
    (newline)
    (yield)
    (if (< n max)
        (loop (+ 1 n))
        (c-exit))))

(define (main)
  (begin
    (fork (do-stuff-n-print "This is A" 3))
    (fork (do-stuff-n-print "This is B" 4))
    (displayln "End")
    (c-exit)))
