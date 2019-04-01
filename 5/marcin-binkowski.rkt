#lang racket

(require rackunit)
(require rackunit/text-ui)

;; procedury pomocnicze
(define (tagged-tuple? tag len x)
  (and (list? x)
       (=   len (length x))
       (eq? tag (car x))))

(define (tagged-list? tag x)
  (and (pair? x)
       (eq? tag (car x))
       (list? (cdr x))))

;; reprezentacja formuł w CNFie
;; zmienne
;formuła jest zmienna jezeli jest symbolem
(define (var? x)
  (symbol? x))

(define (var x)
  x)

(define (var-name x)
  x)
;sprawdza czy x jest blizej poczatku alfabetu niz y
(define (var<? x y)
  (symbol<? x y))

;; literały
;cos jest literalem jezeli jego tag to 'lit drugi element to #f lub #t i trzeci jest zmienna. drugi element oznacza czy literal jest zanegowany
(define (lit pol var)
  (list 'lit pol var))

;konstruktor literału niezanegowanego
(define (pos x)
  (lit true (var x)))
;konstruktor literaly zanegowanego
(define (neg x)
  (lit false (var x)))
;predykat spradzajacy czy lista jest literalem
(define (lit? x)
  (and (tagged-tuple? 'lit 3 x)
       (boolean? (second x))
       (var? (third x))))
;wartosciowanie literalu
(define (lit-pol l)
  (second l))
;selektor zmiennej z literału
(define (lit-var l)
  (third l))

;; klauzule
;cos jest klauzula jezeli jest lista tagowana z symbolem 'clause oraz bierze liste(drugi element klauzuli) i dla kazdego jej elementu sprawdza
;czy jest literalem
(define (clause? c)
  (and (tagged-list? 'clause c)
       (andmap lit? (cdr c))))
;konstruktor klauzuli, bierze liste elementow i robi pare z tagiem 'clause i podana listą
(define (clause . lits)
  (cons 'clause lits))
; seleketor wybierajacy liste literałow z klauzuli
(define (clause-lits c)
  (cdr c))
; cos jest cnf'em jezeli jest lista tagowana z tagiem 'cnf oraz dla listy f kazdy jej element jest klazula
(define (cnf? f)
  (and (tagged-list? 'cnf f)
       (andmap clause? (cdr f))))
;konstruktor cnf'a, tworzy liste tagowaną z tagiem 'cnf i drugim elementem ktorym jest lista klauzul
(define (cnf . clauses)
  (cons 'cnf clauses))
;selektor wybierajacy liste klauzul z cnf'a
(define (cnf-clauses f)
  (cdr f))

;; definicja rezolucyjnych drzew wyprowadzenia
; cos jest axiomem jezeli jest lista tagowana o dlugosci 2 i jej tagiem jest 'axiom
(define (axiom? p)
  (tagged-tuple? 'axiom 2 p))
;konstruktor axiomu/liścia bierze tworzy liste z tagienm 'axiom i klauzula
(define (axiom c)
  (list 'axiom c))
; selektor klauzuli z axiomu
(define (axiom-clause a)
  (second a))
;cos jest resolwenta jezeli jest lista tagowana z tagiem 'resolve o dlugosci 4
(define (res? p)
  (tagged-tuple? 'resolve 4 p))
;konstruktor resolwenty- bierze tworzy liste złozona z tagu 'resolve zmienna wedlug ktorej tworzy rezolwente,
;drzewa wyprowadzenia klauzuli w ktorym zmienna wystepuje  pozytywne, drzewa wyprowadzenia klauzuli w ktorym zmienna wystepuje negatywnie  
(define (res x pf-pos pf-neg)
  (list 'resolve x pf-pos pf-neg))
; selektor rezolwenty - zwraca zmienna wedlug ktorej tworzymy rezolwete
(define (res-var p)
  (second p))
;drzewa wyprowadzenia klauzuli w ktorym zmienna wystepuje  pozytywne
(define (res-proof-pos p)
  (third p))
;drzewa wyprowadzenia klauzuli w ktorym zmienna wystepuje  negatywnie
(define (res-proof-neg p)
  (fourth p))
; cos jest drzewem wyprowadzenia jezeli jest albo( axiomem i  klauzula tego axiomu jest klauzula) albo (jest rezolwenta i zmienna w tej rezolwencie
; jest zmienna i ma dwa prawidlowe drzewa wyprowadzenia dla pozytywnej i negatywnej wartosci zmiennej (res-var)))
(define (proof? p)
  (or (and (axiom? p)
           (clause? (axiom-clause p)))
      (and (res? p)
           (var? (res-var p))
           (proof? (res-proof-pos p))
           (proof? (res-proof-neg p)))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              ZADANIE 1              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (proof-result pf prop-cnf)
  (define (run-axiom c)
    (let ((a (and(member c (cnf-clauses prop-cnf))(clause-lits c))))
      (if (false? a)
          (raise "Literal not in prop-cnf" #f)
          a)))
  (define (run-resolution x clause-positive clause-negative)
    (and
     (findf (lambda (l) (and (lit-pol l)
                             (eq? x (lit-var l))))
            clause-positive)
     (findf (lambda (l) (and (not (lit-pol l))
                             (eq? x (lit-var l))))
            clause-negative)
     (append (remove* (list (lit true x))  clause-positive)
             (remove* (list (lit false x)) clause-negative))))
  
  (define (run-proof-result pf)
    (cond
      [(axiom? pf) (run-axiom (axiom-clause pf))]
      [(res? pf) (run-resolution (res-var pf)
                          (run-proof-result (res-proof-pos pf))
                          (run-proof-result (res-proof-neg pf)))]
      [else false]))
  (let ((x (run-proof-result pf)))
    (if (false? x)
        x
        (if (null? x)
            (clause)
            (clause x)))))

(define (check-proof? pf prop)
  (let ((c (proof-result pf prop)))
    (and (clause? c)
         (null? (clause-lits c)))))

;; XXX: Zestaw testów do zadania pierwszego
(define proof-checking-tests
  (test-suite
   "Tests of check-proof"             
   (test-case
    "Empty proof"
    (check-eq? #f (check-proof? '() (cnf))))
   (test-case
    "'Shallow' proof (one resolution)"
    (check-eq? #t (check-proof? (res 'x(axiom (clause (lit #t 'x)))(axiom(clause (lit #f 'x)))) (cnf (clause (lit #f 'x))(clause (lit #t 'x)))))
    (check-eq? #f (check-proof? (axiom (clause (lit #f 'x))) (cnf (clause (lit #f 'x))))))
   (test-case
    "'Deep' proof (two or more resolutions)"
    (check-eq? #t (check-proof? (res 'q(res 'p (axiom (clause (lit #t 'p) (lit #t 'q)))(axiom (clause (lit #f 'p) (lit #t 'q))))(res 'r (axiom (clause (lit #f 'q) (lit #t 'r)))(axiom (clause (lit #f 'q) (lit #f 'r))))) (cnf (clause (lit #t 'p) (lit #t 'q)) (clause (lit #f 'p) (lit #t 'q)) (clause (lit #f 'q) (lit #t 'r)) (clause (lit #f 'q) (lit #f 'r))))))
   (test-case
    "Wrong resolution"
   (check-eq? #f (check-proof? (res 'z(res 'p (axiom (clause (lit #t 'p) (lit #t 'q)))(axiom (clause (lit #f 'p) (lit #t 'q))))(axiom(clause (lit #f 'q)))) (cnf (clause (lit #f 'q))(clause (lit #t 'p) (lit #t 'q))(clause (lit #f 'p) (lit #t 'q))))))
   ))

(run-tests proof-checking-tests)


;; Wewnętrzna reprezentacja klauzul

(define (sorted? ord? xs)
  (or (null? xs)
      (null? (cdr xs))
      (and (ord? (car xs)
                (cadr xs))
           (sorted? ord? (cdr xs)))))

(define (sorted-varlist? xs)
  (and (andmap var? xs)
       (sorted? var<? xs)))

(define (res-clause pos neg pf)
  (list 'res-clause pos neg pf))

(define (res-clause-pos rc)
  (second rc))
(define (res-clause-neg rc)
  (third rc))
(define (res-clause-proof rc)
  (fourth rc))

(define (res-clause? p)
  (and (tagged-tuple? 'res-clause 4 p)
       (sorted-varlist? (second p))
       (sorted-varlist? (third  p))
       (proof? (fourth p))))

;; implementacja zbiorów / kolejek klauzul do przetworzenia

(define clause-set-empty
  '(stop () ()))

(define (clause-set-add rc rc-set)
  (define (eq-cl? sc)
    (and (equal? (res-clause-pos rc)
                 (res-clause-pos sc))
         (equal? (res-clause-neg rc)
                 (res-clause-neg sc))))
  (define (add-to-stopped sset)
    (let ((procd  (cadr  sset))
          (toproc (caddr sset)))
      (cond
       [(null? procd) (list 'stop (list rc) '())]
       [(or (memf eq-cl? procd)
            (memf eq-cl? toproc))
        sset]
       [else (list 'stop procd (cons rc toproc))])))
  (define (add-to-running rset)
    (let ((pd  (second rset))
          (tp  (third  rset))
          (cc  (fourth rset))
          (rst (fifth  rset)))
      (if (or (memf eq-cl? pd)
              (memf eq-cl? tp)
              (eq-cl? cc)
              (memf eq-cl? rst))
          rset
          (list 'run pd tp cc (cons rc rst)))))
  (if (eq? 'stop (car rc-set))
      (add-to-stopped rc-set)
      (add-to-running rc-set)))

(define (clause-set-done? rc-set)
  (and (eq? 'stop (car rc-set))
       (null? (caddr rc-set))))

(define (clause-set-next-pair rc-set)
  (define (aux rset)
    (let* ((pd  (second rset))
           (tp  (third  rset))
           (nc  (car tp))
           (rtp (cdr tp))
           (cc  (fourth rset))
           (rst (fifth  rset))
           (ns  (if (null? rtp)
                    (list 'stop (cons cc (cons nc pd)) rst)
                    (list 'run  (cons nc pd) rtp cc rst))))
      (cons cc (cons nc ns))))
  (if (eq? 'stop (car rc-set))
      (let ((pd (second rc-set))
            (tp (third  rc-set)))
        (aux (list 'run '() pd (car tp) (cdr tp))))
      (aux rc-set)))

(define (clause-set-done->clause-list rc-set)
  (and (clause-set-done? rc-set)
       (cadr rc-set)))

;; konwersja z reprezentacji wejściowej na wewnętrzną

(define (clause->res-clause cl)
  (let ((pos (filter-map (lambda (l) (and (lit-pol l) (lit-var l)))
                         (clause-lits cl)))
        (neg (filter-map (lambda (l) (and (not (lit-pol l)) (lit-var l)))
                         (clause-lits cl)))
        (pf  (axiom cl)))
    (res-clause (sort pos var<?) (sort neg var<?) pf)))

;; tu zdefiniuj procedury pomocnicze, jeśli potrzebujesz

(define (rc-trivial? rc)
  ;; XXX: uzupełnij
  (error "Not implemented"))

(define (rc-resolve rc1 rc2)
  ;; XXX: uzupełnij
  (error "Not implemented"))

(define (fixed-point op start)
  (let ((new (op start)))
    (if (eq? new false)
        start
        (fixed-point op new))))

(define (cnf->clause-set f)
  (define (aux cl rc-set)
    (clause-set-add (clause->res-clause cl) rc-set))
  (foldl aux clause-set-empty (cnf-clauses f)))

(define (get-empty-proof rc-set)
  (define (rc-empty? c)
    (and (null? (res-clause-pos c))
         (null? (res-clause-neg c))))
  (let* ((rcs (clause-set-done->clause-list rc-set))
         (empty-or-false (findf rc-empty? rcs)))
    (and empty-or-false
         (res-clause-proof empty-or-false))))

(define (improve rc-set)
  (if (clause-set-done? rc-set)
      false
      (let* ((triple (clause-set-next-pair rc-set))
             (c1     (car  triple))
             (c2     (cadr triple))
             (rc-set (cddr triple))
             (c-or-f (rc-resolve c1 c2)))
        (if (and c-or-f (not (rc-trivial? c-or-f)))
            (clause-set-add c-or-f rc-set)
            rc-set))))

(define (prove cnf-form)
  (let* ((clauses (cnf->clause-set cnf-form))
         (sat-clauses (fixed-point improve clauses))
         (pf-or-false (get-empty-proof sat-clauses)))
    (if (eq? pf-or-false false)
        'sat
        (list 'unsat pf-or-false))))

;; XXX: Zestaw testów do zadania drugieg