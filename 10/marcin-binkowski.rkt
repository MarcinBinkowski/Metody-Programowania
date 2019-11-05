#lang racket
; Współpraca Marcin Binkowski, Łukasz Jagnow, Natalia Turek
(require rackunit)
(require rackunit/text-ui)

;; definicja wyrażeń

(struct variable     (x)        #:transparent)
(struct const        (val)      #:transparent)
(struct op           (symb l r) #:transparent)
(struct let-expr     (x e1 e2)  #:transparent)
(struct if-expr      (b t e)    #:transparent)
(struct cons-expr    (l r)      #:transparent)
(struct car-expr     (p)        #:transparent)
(struct cdr-expr     (p)        #:transparent)
(struct pair?-expr   (p)        #:transparent)
(struct null-expr    ()         #:transparent)
(struct null?-expr   (e)        #:transparent)
(struct symbol-expr  (v)        #:transparent)
(struct symbol?-expr (e)        #:transparent)
(struct lambda-expr  (xs b)     #:transparent)
(struct app-expr     (f es)     #:transparent)
(struct apply-expr   (f e)      #:transparent)

(struct closure (xs b env)      #:transparent)


(define (expr? e)
  (match e
    [(variable s)       (symbol? s)]
    [(const n)          (or (number? n)
                            (boolean? n))]
    [(op s l r)         (and (member s '(+ *))
                             (expr? l)
                             (expr? r))]
    [(let-expr x e1 e2) (and (symbol? x)
                             (expr? e1)
                             (expr? e2))]
    [(if-expr b t e)    (andmap expr? (list b t e))]
    [(cons-expr l r)    (andmap expr? (list l r))]
    [(car-expr p)       (expr? p)]
    [(cdr-expr p)       (expr? p)]
    [(pair?-expr p)     (expr? p)]
    [(null-expr)        true]
    [(null?-expr p)     (expr? p)]
    [(symbol-expr v)    (symbol? v)]
    [(symbol?-expr p)   (expr? p)]
    [(lambda-expr xs b) (and (list? xs)
                             (andmap symbol? xs)
                             (expr? b)
                             (not (check-duplicates xs)))]
    [(app-expr f es)    (and (expr? f)
                             (list? es)
                             (andmap expr? es))]
    [(apply-expr f e)   (and (expr? f)
                             (expr? e))]
    [_                  false]))

;; wartości zwracane przez interpreter

(struct val-symbol (s)   #:transparent)

(define (my-value? v)
  (or (number? v)
      (boolean? v)
      (and (pair? v)
           (my-value? (car v))
           (my-value? (cdr v)))
      ; null-a reprezentujemy symbolem (a nie racketowym
      ; nullem) bez wyraźnej przyczyny
      (and (symbol? v) (eq? v 'null))
      (and (closure? v)
           (list? (closure-xs v) ) (andmap symbol? (closure-xs v))
           (expr? (closure-b v))
           (env? (closure-env v))  
      )
      (and ((val-symbol? v) (symbol? (val-symbol-s v))))
      
      ))

;; wyszukiwanie wartości dla klucza na liście asocjacyjnej
;; dwuelementowych list

(define (lookup x xs)
  (cond
    [(null? xs)
     (error x "unknown identifier :(")]
    [(eq? (caar xs) x) (cadar xs)]
    [else (lookup x (cdr xs))]))

;; kilka operatorów do wykorzystania w interpreterze

(define (op-to-proc x)
  (lookup x `(
              (+ ,+)
              (* ,*)
              (- ,-)
              (/ ,/)
              (> ,>)
              (>= ,>=)
              (< ,<)
              (<= ,<=)
              (= ,=)
              (eq? ,(lambda (x y) (eq? (val-symbol-s x)
                                       (val-symbol-s y))))
              )))

;; interfejs do obsługi środowisk

(define (env-empty) null)
(define env-lookup lookup)
(define (env-add x v env) (cons (list x v) env))

(define (env-adds xs vs env)
  (define (it xs vs acc)
    (if (null? xs)
        acc
        (it (cdr xs) (cdr vs) (env-add (car xs) (car vs) acc))))
  (it xs vs env))


(define (env? e)
  (and (list? e)
       (andmap (lambda (xs) (and (list? xs)
                                 (= (length xs) 2)
                                 (symbol? (first xs)))) e)))

;; interpretacja wyrażeń

(define (cons-expr->cons e)
  (cond [(null-expr? e) null]
     [(cons-expr? e)
        (cons
        (cons-expr->cons (cons-expr-l e) )
        (cons-expr->cons (cons-expr-r e )))]
     [else e]))

(define (eval e env)
  (match e
    [(const n) n]
    [(op s l r)
     ((op-to-proc s) (eval l env)
                     (eval r env))]
    [(let-expr x e1 e2)
     (let ((v1 (eval e1 env)))
       (eval e2 (env-add x v1 env)))]
    [(variable x) (env-lookup x env)]
    [(if-expr b t e) (if (eval b env)
                         (eval t env)
                         (eval e env))]
    [(cons-expr l r)
     (let ((vl (eval l env))
           (vr (eval r env)))
       (cons vl vr))]
    [(car-expr p)      (car (eval p env))]
    [(cdr-expr p)      (cdr (eval p env))]
    [(pair?-expr p)    (pair? (eval p env))]
    [(null-expr)       'null]
    [(null?-expr e)    (eq? (eval e env) 'null)]
    [(symbol-expr v)   (val-symbol v)]
    [(lambda-expr xs b) (closure xs b env)]

    [(apply-expr f e1) (eval (app-expr f (cons-expr->cons e1)) env)]

    [(app-expr f es) (let ((vf  (eval f env) )
                           (ves (map (lambda (e) (eval e env) ) es)))
                     (match vf
                       [(closure xs b c-env)
                        (if (=(length xs) (length ves))
                            (eval b (env-adds xs ves c-env))
                            (error "number of applied values does not match number of declared arguments :C"))]
                       [_ (error "application: not a function :(")]))]))

(define (run e)
  (eval e (env-empty)))


(define env1 (env-adds (list 'x 'y) '(1 2) (env-empty)))
; (eval (app-expr (lambda-expr '(x y) (op '+ (variable 'x ) (variable 'y ) )) (list (const 1) (const 2)) ) env1)






(define prac12/13-tests

  (test-suite
   "Tests of lambda-expr, app-expr, apply-expr implementation"

   (test-case
    "App-expr"
    (check-equal?(run (app-expr (lambda-expr '() (const 1)) (env-empty) )) 1)
    (check-equal?(eval (app-expr (lambda-expr '(x y) (op '+ (variable 'x ) (variable 'y ) )) (list (const 1) (const 2))) env1) 3)
    (check-equal?(eval (app-expr (lambda-expr '(a b c d e f)
                                              (op '- (op '+ (variable 'a ) (variable 'b ))
                                                  (op '- (op '* (variable 'c ) (variable 'd )) (op '+ (variable 'e ) (variable 'f)))))
                                                  (list (const 1) (const 2) (const 3) (const 4) (const 5) (const 6))) (env-empty)) 2))

   (test-case
    "Representation cons-expr->cons"
    (equal?(cons-expr (const 1) (cons-expr (const 1) (null-expr))) (list (const 1) (const 1)) )
    (equal?(cons-expr (const 1) (cons-expr (const 2) (cons-expr (const 3) (null-expr)))) (list (const 1) (const 2) (const 3)))
    (equal?(cons-expr->cons (cons-expr (const 1) (cons-expr (const 4)(cons-expr (const 6) (null-expr))))) (list (const 1) (const 4) (const 6))))
  
   (test-case
    "Apply-expr"
    (check-equal? (run (apply-expr (lambda-expr '() (const 1)) (env-empty))) 1)
    (check-equal? (eval (apply-expr (lambda-expr '(x y) (op '+ (variable 'x ) (variable 'y ))) (cons-expr (const 1) (cons-expr (const 2) (null-expr))) )env1)3)
    (check-equal? (run (apply-expr (lambda-expr '(x y z) (op '<= (op '+ (variable 'x ) (variable 'y )) (variable 'z)))
                                    (cons-expr (const 1) (cons-expr (const 2) (cons-expr (const 3) (null-expr)))) ))true)
    (check-equal? (run (apply-expr (lambda-expr '(x) (variable 'x )) (cons-expr (const 1)(null-expr))))1))
   )) 

(run-tests prac12/13-tests)
