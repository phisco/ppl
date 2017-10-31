#lang racket

;;continuations and macros

(define-syntax when
  (syntax-rules ()
    ((when condition head . body)
     ;; head allow to require at least a parameter
     (if condition (begin head . body) #f))))
;; begin does not expect a  list, so we need to unpack it
;; . operator (begin . ( () () () )) -> (begin () () () () )

;; (1 . < . 2) -> (< 1 2) 

(define (answer? x)
  (= x 42))

(when (answer? 42)
  (newline)
  (display "The answer!!\n"))


(define (test a . b)
  (begin (display a)
         (display b))
  )

(define-syntax mtoa
  (syntax-rules ()
    ((mtoa fun arg) (if)))) ;;trick to require mtoa at least to have 2 parameters

(define-syntax repeat
  (syntax-rules (until)
               ((_ body ... until cond)
                (let loop ()
                 (begin
                   body ...
                   (unless cond
                     (loop))) ;;unless is a if without  else condition
                 ))))

(let ((x 5))
  (repeat
   (display x)
   (newline)
   (set! x (sub1 x))
   until (zero? x)))

;;macro to implement or with a cond
(define-syntax ouror
  (syntax-rules()
    [(ouror) #f]
    [(ouror e1 e2 ...)
     (cond [e1 #t] ;; e1 is a condition that should be matched to return the second argument (#t here)
           [else (ouror e2 ...)])]
    ))

;; coroutines
(define *queue* '())

(define (empty-queue?)
  (null? *queue*))

(define (enqueue x)
  (set! *queue* (append *queue* (list x))))

(define (dequeue)
  (let ((x (car *queue*)))
    (set! *queue* (cdr *queue*))
    x)
  )

(define (fork proc) ;;procedures have side effects, functions don't
  (call/cc
   (λ(cc)
     (enqueue cc)
     (proc))))

(define (yield)
  (call/cc
   (λ(cc)
     (enqueue cc)
     ((dequeue))))) ;;element returned by dequeue has to be evaluated (())

(define (cexit prov) ;; x stands for provenence
  (if (empty-queue?)
      (exit)
      (begin
        (displayln prov)
        ((dequeue)))
      )
  )

(define (main)
  (fork  (do-stuff "This is A" 3))
  (fork  (do-stuff "This is B" 4))
  (displayln "END")
  (cexit "Main")
)

(define ((do-stuff str max))
  (let loop ((n 0))
    (display str)
    (display " ")
    (display n)
    (newline)
    (yield)
    (if (< n max)
        (loop (+ 1 n))
        (cexit (string-append "do-stuff " str "\n")))))
;;if no cexit, we end up executing again the code  after (do-stuff a 3) so do-stuff b 4 again
