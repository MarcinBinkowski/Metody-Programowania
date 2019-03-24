#lang racket
;Współpraca: Natalia Turek, Łukasz Jagnow, Marcin Binkowski

(require rackunit)
(require rackunit/text-ui)


(define (inc n)
  (+ n 1))

;;; tagged lists
(define (tagged-list? len-xs tag xs)
  (and (list? xs)
       (= len-xs (length xs))
       (eq? (first xs) tag)))

;;; ordered elements
(define (make-elem pri val)
  (cons pri val))

(define (elem-priority x)
  (car x))

(define (elem-val x)
  (cdr x))

;;; leftist heaps (after Okasaki)

;; data representation
(define leaf 'leaf)

(define (leaf? h) (eq? 'leaf h))

(define (hnode? h)
  (and (tagged-list? 5 'hnode h)
       (natural? (caddr h))))

(define (make-hnode elem heap-a heap-b)
  (if(> (rank heap-a) (rank heap-b))
     (list 'hnode elem (inc (rank heap-b)) heap-a heap-b)
     (list 'hnode elem (inc (rank heap-a)) heap-b heap-a)))

(define (hnode-elem h)
  (second h))

(define (hnode-left h)
  (fourth h))

(define (hnode-right h)
  (fifth h))

(define (hnode-rank h)
  (third h))

(define (hord? p h)
  (or (leaf? h)
      (<= p (elem-priority (hnode-elem h)))))

(define (heap? h)
  (or (leaf? h)
      (and (hnode? h)
           (heap? (hnode-left h))
           (heap? (hnode-right h))
           (<= (rank (hnode-right h))
               (rank (hnode-left h)))
           (= (rank h) (inc (rank (hnode-right h))))
           (hord? (elem-priority (hnode-elem h))
                  (hnode-left h))
           (hord? (elem-priority (hnode-elem h))
                  (hnode-right h)))))

(define (rank h)
  (if (leaf? h)
      0
      (hnode-rank h)))

;; operations

(define empty-heap leaf)

(define (heap-empty? h)
  (leaf? h))

(define (heap-insert elt heap)
  (heap-merge heap (make-hnode elt leaf leaf)))

(define (heap-min heap)
  (hnode-elem heap))

(define (heap-pop heap)
  (heap-merge (hnode-left heap) (hnode-right heap)))

(define (heap-merge h1 h2)
  (cond
   [(leaf? h1) h2]
   [(leaf? h2) h1]
   ;; XXX: fill in the implementation
   [else
    (if (<=(elem-priority (heap-min h1)) (elem-priority (heap-min h2)))
        (make-hnode (heap-min h1) (hnode-left h1) (heap-merge (hnode-right h1) h2))
        (make-hnode (heap-min h2) (hnode-left h2) (heap-merge h1 (hnode-right h2))))]))


;;; heapsort. sorts a list of numbers.
(define (heapsort xs)
  (define (popAll h)
    (if (heap-empty? h)
        null
        (cons (elem-val (heap-min h)) (popAll (heap-pop h)))))
  (let ((h (foldl (lambda (x h)
                    (heap-insert (make-elem x x) h))
            empty-heap xs)))
    (popAll h)))

;;; check that a list is sorted (useful for longish lists)
(define (sorted? xs)
  (cond [(null? xs)              true]
        [(null? (cdr xs))        true]
        [(<= (car xs) (cadr xs)) (sorted? (cdr xs))]
        [else                    false]))

;;; generate a list of random numbers of a given length
(define (randlist len max)
  (define (aux len lst)
    (if (= len 0)
        lst
        (aux (- len 1) (cons (random max) lst))))
  (aux len null))


(define heapsort-tests
  (test-suite
   "Tests of heapsort implementation"
                
   (test-case
    "Small lists sort"
      (check-pred sorted? (heapsort '(1 43 123 12 1 4)))
      (check-pred sorted? (heapsort '(-23 13 -12 -4 1 1 0 0 0 0)))
      (check-pred sorted? (heapsort '(1 2 1))))
   (test-case
    "Null list sort"
    (check-pred sorted? (heapsort '())))
   (test-case
    "Big list sort"
      (check-pred sorted? (heapsort(randlist 1000 1000000)))
      (check-pred sorted? (heapsort(randlist 2000 1000000))))
    (test-case
     "Very Big list sort"
       (check-pred sorted? (heapsort (randlist 25000 1000000)))
       (check-pred sorted? (heapsort (randlist 25000 1000000))))
    (test-case
     "Same number in list"
       (check-pred sorted? (heapsort (randlist 25000 1))))
    (test-case
     "Negative numbers in list"
     (check-pred sorted? (heapsort '(-4 1 2 10000 -4 3 0 -1 2 24 5 123 -5100))))
    (test-case
     "Test heap building"
     (check-pred heap? (foldl (λ (x h) (heap-insert (make-elem x x) h)) empty-heap (randlist 20 100)))
     (check-pred heap? (foldl (λ (x h) (heap-insert (make-elem x x) h)) empty-heap (randlist 1000 10000))))
    ))

(run-tests heapsort-tests)
