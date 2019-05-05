#lang racket
; Wspolpraca Marcin Binkowski, Łukasz Jagnow, Natalia Turek 
(require rackunit)
(require rackunit/text-ui)

(provide (all-defined-out))

;; definicja wyrażeń z let-wyrażeniami i if-wyrażeniami

(struct variable (x)         #:transparent)
(struct const    (val)       #:transparent)
(struct op       (symb l r)  #:transparent)
(struct let-expr (x e1 e2)   #:transparent)
(struct if-expr  (b t e)     #:transparent)

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
    [_                  false]))

;; definicja instrukcji w języku WHILE

(struct skip      ()       #:transparent) ; skip
(struct comp      (s1 s2)  #:transparent) ; s1; s2
(struct assign    (x e)    #:transparent) ; x := e
(struct while     (b s)    #:transparent) ; while (b) s
(struct if-stm    (b t e)  #:transparent) ; if (b) t else e
(struct var-block (x e s)  #:transparent) ; var x := e in s

(define (stm? e)
  (match e
    [(skip) true]
    [(comp s1 s2)   (and (stm? s1) (stm? s2))]
    [(assign x e)   (and (symbol? x) (expr? e))]
    [(while b s)    (and (expr? b) (stm? s))]
    [(if-stm b t e) (and (expr? b) (stm? t) (stm? e))]
    [_ false]))
  
;; wyszukiwanie wartości dla klucza na liście asocjacyjnej
;; dwuelementowych list
(define (lookup x xs)
  (cond
    [(null? xs)
     (error x "unknown identifier :(")]
    [(eq? (mcar(mcar xs)) x) (mcdr(mcar xs))]
    [else (lookup x (mcdr xs))]))
;; aktualizacja środowiska dla danej zmiennej (koniecznie już
;; istniejącej w środowisku!)

(define (set-nth! n v xs)
  (if (null? xs)
      xs
      (if (= 0 n)
          (set-mcdr!(mcar xs) v)
          (set-nth! (- n 1) v (mcdr xs) )))
  xs)

(define (update x v xs)
  (define (iter i ys)
    (cond
      [(null? ys) 
       (error x "unknown identifier :(")]
      [(eq? (mcar (mcar ys)) x) i]
      [else
       (iter (+ i 1) (mcdr ys))]))
  (set-nth! (iter 0 xs) v xs))

;; kilka operatorów do wykorzystania w interpreterze

(define (op-to-proc x)
  (lookup x (mcons
             (mcons '+ +)
             (mcons
              (mcons '* *)
              (mcons
               (mcons '- -)
               (mcons
                (mcons '/ /)
                (mcons
                 (mcons '% modulo)
                 (mcons
                  (mcons '> >)
                  (mcons
                   (mcons '>= >=)
                   (mcons
                    (mcons '< <)
                    (mcons
                     (mcons '<= <=)
                     (mcons
                      (mcons '= =)
                      (mcons
                       (mcons '!= (lambda (x y) (not (= x y))))
                       (mcons
                        (mcons '&& (lambda (x y) (and x y)))
                        (mcons
                         (mcons '|| (lambda (x y) (or x y))) '())))))))))))))))
;; interfejs do obsługi środowisk

(define (env-empty) null)
(define env-lookup lookup)
(define (env-add x v env) (mcons (mcons x v) env))        
(define env-update update)                                
(define env-discard mcdr)                                 
(define (env-from-assoc-list xs) (list->mlist xs))        

(define (list->mlist xs)
  (define (iter zs)
    (if (null? zs)
        zs
        (mcons (mcons (car zs) (car(cdr zs))) (iter (cdr(cdr zs))) )))
  (let ((ys (flatten xs)))
    (iter ys)))
;; ewaluacja wyrażeń ze środowiskiem

(define (eval e env)
  (match e
    [(const n) n]
    [(op s l r) ((op-to-proc s) (eval l env)
                                (eval r env))]
    [(let-expr x e1 e2)
     (let ((v1 (eval e1 env)))
       (eval e2 (env-add x v1 env)))]
    [(variable x) (env-lookup x env)]
    [(if-expr b t e) (if (eval b env)
                         (eval t env)
                         (eval e env))]))

;; interpretacja programów w języku WHILE, gdzie środowisko m to stan
;; pamięci. Interpreter to procedura, która dostaje program i początkowy
;; stan pamięci, a której wynikiem jest końcowy stan pamięci. Pamięć to
;; aktualne środowisko zawierające wartości zmiennych

(define (interp p m)
  (match p
    [(skip) m]
    [(comp s1 s2) (interp s2 (interp s1 m))]
    [(assign x e)
     (env-update x (eval e m) m)]
    [(while b s)
     (if (eval b m)
         (interp p (interp s m))
         m)]
    [(var-block x e s)
     (env-discard
       (interp s (env-add x (eval e m) m)))]
    [(if-stm b t e) (if (eval b m)
                        (interp t m)
                        (interp e m))]))

;; silnia zmiennej is

(define fact-in-WHILE
  (var-block 'x (const 0)                                           ; var x := 0 in
     (comp (assign 'x (const 1))                                    ;   x := 1
     (comp (while (op '> (variable 'i) (const 0))                   ;   while (i > 0)
              (comp (assign 'x (op '* (variable 'x) (variable 'i))) ;     x := x * i
                    (assign 'i (op '- (variable 'i) (const 1)))))   ;     i := i - 1
           (assign 'i (variable 'x))))))                            ;   i := x

(define (factorial-using-WHILE n)
  (env-lookup 'i (interp fact-in-WHILE
                         (env-from-assoc-list `((i ,n))))))

;; najmniejsza liczba pierwsza nie mniejsza niż i
(define square-in-WHILE
  (var-block 'x (op '* (variable 'i) (variable 'i))
  (assign 'i (variable 'x))))
             
(define (square-using-WHILE n)
  (env-lookup 'i (interp square-in-WHILE
                         (env-from-assoc-list `((i ,n))))))

(define find-prime-in-WHILE
 (var-block 'c (variable 'i)                                         ; var c := i in
 (var-block 'continue (const true)                                   ; var continue := true in
 (comp
  (while (variable 'continue)                                        ; while (continue)
   (var-block 'is-prime (const true)                                 ;   var is-prime := true in
   (var-block 'x (const 2)                                           ;   var x := 2 in
    (comp
     (while (op '&& (variable 'is-prime)                             ;   while (is-prime &&
                    (op '< (variable 'x) (variable 'c)))             ;            x < c)
        (comp (if-stm (op '= (op '% (variable 'c) (variable 'x))     ;     if (c % x =
                             (const 0))                              ;                 0)
                      (assign 'is-prime (const false))               ;       is-prime := false
                      (skip))                                        ;     else skip
              (assign 'x (op '+ (variable 'x) (const 1)))))          ;     x := x + 1 
     (if-stm (variable 'is-prime)                                    ;   if (is-prime)
             (assign 'continue (const false))                        ;     continue := false
             (comp (assign 'continue (const true))                   ;   else continue := true
                   (assign 'c (op '+ (variable 'c) (const 1))))))))) ;        c := c + 1
  (assign 'i (variable 'c))))))                                      ; i := c

(define (find-prime-using-WHILE n)
  (env-lookup 'i (interp find-prime-in-WHILE
                         (env-from-assoc-list `((i ,n) (is-prime ,true))))))          

;; porownajmy wydajnosc!

;; ten sam algorytm do wyszukiwania liczby pierwszej nie mniejszej niż n
;; zapisany funkcyjnie jest dosc brzydki, ale odpowiada temu zapisanemu w WHILE

(define (find-prime-native n)
  (define (is-prime c isp x)
    (if (and isp (< x c))
      (if (= (modulo c x) 0)
          (is-prime c false (+ x 1))
          (is-prime c isp   (+ x 1)))
      isp))
  (if (is-prime n true 2)
      n
      (find-prime-native (+ n 1))))

;; testujemy, żeby dowiedzieć się, jak dużo wydajności tracimy przez
;; uruchamianie programu w naszym interpreterze

(define (test)
  (begin
    (display "wait...\n")
    (flush-output (current-output-port))
    (test-performance)))
  
(define (test-performance)
  (let-values
      (((r1 cpu1 real1 gc1) (time-apply find-prime-using-WHILE (list 111111)))
       ((r2 cpu2 real2 gc2) (time-apply factorial-using-WHILE (list 10000))))
    (begin
      (display "WHILE Prime  time (cpu, real, gc): ")
      (display cpu1)  (display ", ")
      (display real1) (display ", ")
      (display gc1)   (display "\n")
      (display "WHILE Factorial time (cpu, real, gc): ")
      (display cpu2)  (display ", ")
      (display real2) (display ", ")
      (display gc2)   (display "\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Testy do zadania 10

(define WHILE-mcons-env-tests
  (test-suite
   "Tests of new WHILE environment representation implementation"
                
   (test-case
    "Factorial tests"
      (check-equal? (factorial-using-WHILE 0) 1)
      (check-equal? (factorial-using-WHILE 1) 1)
      (check-equal? (factorial-using-WHILE 10) 3628800))
     
   (test-case
    "Find prime tests"
    (check-equal? (find-prime-using-WHILE 1) (find-prime-native 1))
    (check-equal? (find-prime-using-WHILE 2451) (find-prime-native 2451))
    (check-equal? (find-prime-using-WHILE 97677) (find-prime-native 97677)))

   (test-case
    "Square tests"
    (check-equal? (square-using-WHILE 1) (* 1 1))
    (check-equal? (square-using-WHILE 2) (* 2 2))
    (check-equal? (square-using-WHILE 0) (* 0 0))
    (check-equal? (square-using-WHILE 115556) (* 115556 115556)))))
    

;(run-tests WHILE-mcons-env-tests)