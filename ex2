#lang racket
(struct leaf
  ((content #:mutable) empty))

(define (display-leaf l)
  (if (leaf? l)
      (display (leaf-content l))
      (display "Not a leaf")
      ))

(struct node leaf ;inheritance
  (left right))

(define (create-empty) (leaf -1 #t))

(define (create-node v t1 t2)
  (node v #f t1 t2))

(define  (create-leaf v) (node  v #f (create-empty)(create-empty)))

(define (display-tree t)
  (if (not (node? t))
      (if (leaf-empty t)
          (display "-----\n")   ;always create empty leaf so always print ----
          (begin
            (display "Leaf \n")
            (display (leaf-content t))   ;node-content doesn't work, inheritanche does generate
                                         ;accessors
            (newline)))
      (begin
        (display "Node")
        (display (leaf-content t))
        (newline)
        (display-tree (node-left t))
        (display-tree (node-right t)))))

(define l1 (create-leaf 10))

(define l2 (create-leaf 12))
(define n1 (create-node 20 l1 l2 ))
;;(displa-tree n1)
;;macros

(define-syntax ++
  (syntax-rules ()
    ((_ i)
    (let ((a 0))
      (set! i (+ 1 i))))))

(define-syntax **
  (syntax-rules ()
    ((_ i)   ;; _ may be any kind of 
     (let ((a i))
       (set! i (* a i))))))

(define (fact n)
  (foldl (λ (i x) (* i x)) 1 (range 1 (+ 1 n))))

(define (fib n)
  (cond
    [(= n 0) 0]
    [(= n 1) 0]
    [(= n 2) 1]
    [else  (+ (fib (- n 1)) (fib (- n 2)))]
    ))

(define (choose i . args) (
           (if (zero? i)
               (car args)
               (apply choose (- i 1) (cdr args)))))

;(choose 1 (fact 5) (fact 40))
; scheme is eager so it evaluates in parallel all the parameters, but we can wrap each argument in a lamba
; this is called thunking
;(choose 1 (λ () (fact 5)) (λ () (+ 1 2)) (λ () (fact 40)))
;faster than
;(choose 1 (fact 5) (+ 1 2) (fact 40))
; evaluates only the needed one

(define-syntax choose-m
  (syntax-rules ()
    ((_ i f) f)
    ((_ i f f1 ...)
     (if (zero? i)
         f
         (choose-m (sub1  i) f1 ...)))))
;(choose-m 1 (fact 5) (+ 1 2) (fact 40))
;is much faster than 


(define (right-now)
  (call/cc
   (λ (cc)
     (cc 1)) ;cc also colled usually escape because if no argument passed it escape, otherwise returns the argument passed
   ))

(define (break-test)
  (call/cc (λ (break)
             (for-each (λ (i) (if (= (remainder i 2) 0)
                                  (break) ; like that it breaks the function evaluating (break), only "break" would return the continuation, breaking this iteration like continue
                                  (display i)
                                  ))
             '(1 2 3 4 5)))))

(define (continue-test)
  (for-each (λ (i)
              (call/cc (λ (continue)
                         (if (= 0 (remainder i 2))
                             (continue)
                             (display i)
                             ))))))

