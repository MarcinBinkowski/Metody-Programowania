#lang racket
; Współpraca Marcin Binkowski, Natalia Turek, Łukasz Jagnow
;; sygnatura: grafy
(define-signature graph^
  ((contracted
    [graph        (-> list? (listof edge?) graph?)]
    [graph?       (-> any/c boolean?)]
    [graph-nodes  (-> graph? list?)]
    [graph-edges  (-> graph? (listof edge?))]
    [edge         (-> any/c any/c edge?)]
    [edge?        (-> any/c boolean?)]
    [edge-start   (-> edge? any/c)]
    [edge-end     (-> edge? any/c)]
    [has-node?    (-> graph? any/c boolean?)]
    [outnodes     (-> graph? any/c list?)]
    [remove-node  (-> graph? any/c graph?)]
    )))

;; prosta implementacja grafów
(define-unit simple-graph@
  (import)
  (export graph^)

  (define (graph? g)
    (and (list? g)
         (eq? (length g) 3)
         (eq? (car g) 'graph)))

  (define (edge? e)
    (and (list? e)
         (eq? (length e) 3)
         (eq? (car e) 'edge)))

  (define (graph-nodes g) (cadr g))

  (define (graph-edges g) (caddr g))

  (define (graph n e) (list 'graph n e))

  (define (edge n1 n2) (list 'edge n1 n2))

  (define (edge-start e) (cadr e))

  (define (edge-end e) (caddr e))

  (define (has-node? g n) (not (not (member n (graph-nodes g)))))
  
  (define (outnodes g n)
    (filter-map
     (lambda (e)
       (and (eq? (edge-start e) n)
            (edge-end e)))
     (graph-edges g)))

  (define (remove-node g n)
    (graph
     (remove n (graph-nodes g))
     (filter
      (lambda (e)
        (not (eq? (edge-start e) n)))
      (graph-edges g)))))

;; sygnatura dla struktury danych
(define-signature bag^
  ((contracted
    [bag?       (-> any/c boolean?)]
    [empty-bag  (and/c bag? bag-empty?)]
    [bag-empty? (-> bag? boolean?)]
    [bag-insert (-> bag? any/c (and/c bag? (not/c bag-empty?)))]
    [bag-peek   (-> (and/c bag? (not/c bag-empty?)) any/c)]
    [bag-remove (-> (and/c bag? (not/c bag-empty?)) bag?)])))

;; struktura danych - stos
(define-unit bag-stack@
  (import)
  (export bag^)

  (define (bag? b)
    (list? b))
  (define (bag-empty? b)
    (null? b))
  (define empty-bag '())
  (define (bag-insert b elem)
    (cons elem b))
  (define (bag-peek b)
    (car b))
  (define (bag-remove b)
    (cdr b))
;; TODO: zaimplementuj stos
)

;; struktura danych - kolejka FIFO
;; do zaimplementowania przez studentów
(define-unit bag-fifo@
  (import)
  (export bag^)

  (define (bag? b)
    (and
     (cons? b)
     (list? (car b))
     (list? (cdr b))))
  (define (bag-empty? b)
      (and (null? (car b))(null? (cdr b))))
  (define empty-bag (cons '()  '()))
  (define (bag-insert b elem)
    (cons (cons elem (car b)) (cdr b)))
  (define (bag-peek b)
    (if (null? (cdr b))
        (car (reverse (car b)))
        (car (cdr b))))
  (define (bag-remove b)
    (if (null? (cdr b))
        (cons null (cdr (reverse (car b))))
        (cons (car b) (cdr(cdr b)))))
  
;; TODO: zaimplementuj kolejkę
)

;; sygnatura dla przeszukiwania grafu
(define-signature graph-search^
  (search))

;; implementacja przeszukiwania grafu
;; uzależniona od implementacji grafu i struktury danych
(define-unit/contract graph-search@
  (import bag^ graph^)
  (export (graph-search^
           [search (-> graph? any/c (listof any/c))]))
  (define (search g n)
    (define (it g b l)
      (cond
        [(bag-empty? b) (reverse l)]
        [(has-node? g (bag-peek b))
         (it (remove-node g (bag-peek b))
             (foldl
              (lambda (n1 b1) (bag-insert b1 n1))
              (bag-remove b)
              (outnodes g (bag-peek b)))
             (cons (bag-peek b) l))]
        [else (it g (bag-remove b) l)]))
    (it g (bag-insert empty-bag n) '()))
  )

;; otwarcie komponentu grafu
(define-values/invoke-unit/infer simple-graph@)

;; graf testowy
(define test-graph
  (graph
   (list 1 2 3 4)
   (list (edge 1 3)
         (edge 1 2)
         (edge 2 4))))
;; TODO: napisz inne testowe grafy!
(define test-graph-1
  (graph
   (list 1 2)
   (list (edge 1 2))))

(define test-graph-2
  (graph
   (list 1 2 3 4 )
   (list (edge 1 2)
         (edge 2 3)
         (edge 3 4)
         (edge 1 4))))

(define test-graph-3
  (graph
   (list 1 2 3 4 5)
   (list (edge 1 5)
         (edge 2 5)
         (edge 3 5)
         (edge 4 5))))

(define test-graph-4
  (graph
   (list 1 2 3 4 5 6)
   (list (edge 1 2)
         (edge 1 3)
         (edge 1 4)
         (edge 1 6)
         (edge 2 3)
         (edge 3 4)
         (edge 4 5)
         (edge 4 6)
         (edge 5 6))))

(define test-graph-5
  (graph
   (list 1 2 3 4 5 6)
   (list (edge 1 2)
         (edge 2 3)
         (edge 3 4)
         (edge 4 5)
         (edge 5 6)
         (edge 1 6)
         (edge 1 4)
         (edge 3 6))))


;; otwarcie komponentu stosu
(define-values/invoke-unit/infer bag-stack@)
;; opcja 2: otwarcie komponentu kolejki
;(define-values/invoke-unit/infer bag-fifo@)

;; testy w Quickchecku
(require quickcheck)

;; test przykładowy: jeśli do pustej struktury dodamy element
;; i od razu go usuniemy, wynikowa struktura jest pusta
(quickcheck
 (property ([s arbitrary-symbol])
           (bag-empty? (bag-remove (bag-insert empty-bag s)))))
;; TODO: napisz inne własności do sprawdzenia!
;; jeśli jakaś własność dotyczy tylko stosu lub tylko kolejki,
;; wykomentuj ją i opisz to w komentarzu powyżej własności
(quickcheck
 (property ([s arbitrary-symbol])
           (bag? (bag-insert empty-bag s))))

(quickcheck
 (property ([s1 arbitrary-symbol]
            [s2 arbitrary-symbol])
           (bag-empty?(bag-remove(bag-insert (bag-remove (bag-insert empty-bag s1)) s2)))))
(quickcheck
 (property ([s1 arbitrary-symbol]
            [s2 arbitrary-symbol])
           (bag-empty?(bag-remove(bag-insert (bag-remove (bag-insert empty-bag s1)) s2)))))

(quickcheck
 (property ([s1 arbitrary-symbol]
            [s2 arbitrary-symbol]
            [s3 arbitrary-symbol])
           (bag? (bag-remove(bag-insert (bag-insert (bag-insert empty-bag s1) s2) s3)))))

; Stos działa na zasadzie: Ostatni element wstawiony jest pierwszym  niego wyciągniętym. Więc po wstawieniu najpierw s1 potem s2 i
; usunięciu ze stosu elementu zostajemy z s1.
(display "Stack test ")
(quickcheck
 (property ([s1 arbitrary-symbol]
            [s2 arbitrary-symbol])
           (equal? s1 (bag-peek (bag-remove(bag-insert(bag-insert empty-bag s1)s2))))))
; First in First out - pierwszy element, który wstawimy do kolejki powininen być pierwszym który zwrócimy, więc po usunięciu go
; powinniśmy dostać element wstawiony jako drugi
(display "Fifo queue test ")
(quickcheck
 (property ([s1 arbitrary-symbol]
            [s2 arbitrary-symbol])
           (equal? s2 (bag-peek (bag-remove(bag-insert(bag-insert empty-bag s1)s2))))))
;; otwarcie komponentu przeszukiwania
(define-values/invoke-unit/infer graph-search@)

;; uruchomienie przeszukiwania na przykładowym grafie
(search test-graph 1)
;; TODO: uruchom przeszukiwanie na swoich przykładowych grafach!(search test-graph-1 1)
(search test-graph-1 1)
(search test-graph-2 4)
(search test-graph-3 3)
(search test-graph-4 1)
(search test-graph-5 2)