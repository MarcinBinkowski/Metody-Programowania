#lang racket
;Współpraca Łukasz Jagnow, Natalia Turek, Marcin Binkowski

(define (head xs)
  (car xs))

(define (tail xs)
  (cdr xs))

(define (inc x)
  (+ x 1))

(define (map f xs)
  (if (null? xs)
      null
      (cons
       (f (head xs))
       (map f (tail xs)))))

(define (filter p? xs)
  (if (null? xs)
      null
      (if (p? (head xs))
          (cons (head xs) (filter p? (tail xs)))
          (filter p? (tail xs)))))


