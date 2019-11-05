#lang racket
;Współpraca: Marcin Binkowski, Łukasz Jagnow, Natalia Turek
(require rackunit)
(require rackunit/text-ui)

(struct const(value) #:transparent)
(struct op (symb l r) #:transparent)
; operator unarny zamiast struktury reprezentującej pochodną umożliwia łatwiejsze rozwijanie programu w przyszłości
(struct op-unary (symb fun) #:transparent) 
(struct variable () #:transparent)

(define (expr? e)
  (match e
    [(variable) true]
    [(const n)  (number? n)]
    [(op symb l r) (and (member symb '(+ *))
                     (expr? l)
                     (expr? r))]
    [(op-unary symb fun)(and (member symb '(∂))
                                  (expr? fun))]
    [_          false]))


(define (∂ f)
  (match f
    [(const n)   (const 0)]
    [(variable)  (const 1)]
    [(op '+ f g) (op '+ (∂ f) (∂ g))]
    [(op '* f g) (op '+ (op '* (∂ f) g)
                        (op '* f (∂ g)))]
    [(op-unary '∂ fun) (∂ (∂ fun))]))


(define (eval e (value 0))
  (match e
    
    [(const n) n]
    [(variable) value]
    [(op '+ l r) (+ (eval l value) (eval r value))]
    [(op '* l r) (* (eval l value) (eval r value))]
    [(op-unary '∂ fun) (eval (∂ fun) value)]))

;Zestaw testów

(define eval-tests
  (test-suite
   "Tests of evaluation implementation"
    (test-case
     "Tessts of expr? of basic cases"
     (check-equal? (expr? (variable)) true)
     (check-equal? (expr? (const 2)) true)
     (check-equal? (expr? (op '* (const 2) (const 3))) true)
     (check-equal? (expr? (op '+ (const 2) (const 3))) true)
     (check-equal? (expr? (op-unary '∂ (const 2))) true))
    (test-case
     "Tests of derivative"
     (check-equal? (∂ (const 2)) (const 0))
     (check-equal? (∂ (variable)) (const 1))
     (check-equal? (∂ (op '+ (variable) (const 1))) (op '+ (const 1) (const 0)))
     (check-equal? (∂ (op '* (variable) (const 1))) (op '+ (op '* (const 1) (const 1)) (op '* (variable) (const 0)))))
    (test-case
     "Tests of evaluation of basic cases"
     (check-equal? (eval (variable)0) 0)
     (check-equal? (eval (const 2)) 2)
     (check-equal? (eval (op '* (const 2) (const 3))) 6)
     (check-equal? (eval (op '+ (const 2) (const 3))) 5)
     (check-equal? (eval (op-unary '∂ (const 2))) 0))
    (test-case
    "Tests of simple expressions evaluation"
    (check-equal? (eval (op '+ (op '* (variable) (variable)) (const 10)) 5) 35 )
    (check-equal? (eval (op-unary '∂ (op '+ (op '* (variable) (variable)) (const 10))) 5) 10))
    (test-case
     "Tests of complex expressions evaluation"
    (check-equal? (eval (op-unary '∂(op-unary '∂ (op '* (op '* (variable) (variable)) (variable)))) 5) 30)
    (check-equal? (eval (op '* (op-unary '∂ (op '+ (const 1) (op '* (variable)(const 4))))(op-unary '∂ (const 1231))) 123) 0)
    (check-equal? (eval (op '* (op '* (op '+ (const 1) (op '+ (const 1) (const 1))) (op '* (variable)(const 2))) (variable)) 2) 24)
    )))


(run-tests eval-tests)

