#lang racket

(require rackunit)
(require rackunit/text-ui)

; Współpraca Marcin Binkowski, Natalia Turek i Łukasz Jagnow 
(define (head list1) (car list1))
(define (tail list1) (cdr list1))

(define (merge list1 list2)
  (if (empty? list1)
      list2
      (if (empty? list2)
          list1
          (if (<= (head list1) (head list2))
              (cons (head list1) (merge (tail list1) list2))
              (cons (head list2) (merge list1 (tail list2)))))))
    
(define(split list1)
  (define (iter list1 acc1 len//2)
    (if(<= len//2 0)
       (list acc1 list1)
          (iter (tail list1) (cons (head list1) acc1) (- len//2 1))
          ))
  (let ((len//2 (ceiling (/ (length list1) 2))))
        (iter list1 '() len//2))
  )

(define (mergesort list1)
  (if(null? list1)
     list1
     (if(null? (cdr list1))
        list1
        (let((pair-of-lists (split list1)))
         (merge (mergesort (car pair-of-lists))(mergesort (cadr pair-of-lists)))
  ))))



(define (randomlist n mx) ;https://stackoverflow.com/questions/40497748/building-a-random-list. User rsno answer. Used only for generating big random lists
  (for/list ((i n))
    (add1 (random mx))))

(define mergesort-tests
  (test-suite
   "Tests of mergesort implementation"
                
   (test-case
    "Small lists sort"
      (check-equal? (mergesort '(1 43 123 12 1 4)) (sort  '(1 43 123 12 1 4) <))
      (check-equal? (mergesort '(-23 13 -12 -4 1 1 0 0 0 0)) (sort  '(-23 13 -12 -4 1 1 0 0 0 0) <))
      (check-equal? (mergesort '(1 2 1)) (sort  '(1 2 1) <)))
   (test-case
    "Null list sort"
    (check-equal? (mergesort '()) (sort  '() <)))
   (test-case
    "Big list sort"
    (let ((big-list  (randomlist 1000 1000000)))
      (check-equal? (mergesort big-list) (sort  big-list <)))
    (let ((big-list  (randomlist 2000 1000000)))
      (check-equal? (mergesort big-list) (sort  big-list <))))
    (test-case
     "Very Big list sort"
     (let ((big-list  (randomlist 500000 1000000)))
       (check-equal? (mergesort big-list) (sort  big-list <)))
     (let ((big-list  (randomlist 500000 1000000)))
       (check-equal? (mergesort big-list) (sort  big-list <))))
    (test-case
     "Only 1s in list"
     (let ((big-list  (randomlist 500000 1)))
       (check-equal? (mergesort big-list) (sort  big-list <))))
    (test-case
     "Negative numbers in list"
     (check-equal? (mergesort '(-4 1 2 10000 -4 3 0 -1 2 24 5 123 -5100)) (sort   '(-4 1 2 10000 -4 3 0 -1 2 24 5 123 -5100) <)))))


;(run-tests mergesort-tests) ;to run tests (takes about 15 seconds to finish)