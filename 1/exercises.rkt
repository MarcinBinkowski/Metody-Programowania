#lang Racket

;4.
(define (max_dist x y z)
   (if (> x y)
       (if (> z y)
           (+ (* z z) (* x x))
           (+ (* y y) (* x x)))
       (if (> x z)
           (+ (* y y) (* x x))
           (+ (* z z) (* y )))))

;5
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

;6
;(or (< 1 3) (/ 2 0))
;(and (< 5 3) (/ 2 0))

;7
(define (p) (p))
(define (test x y)
  (if (= x )
      0
      y))
;8
(define (power-close-to b n)
  (define (iter e)
    (cond
      [(- b 1) NULL]
      [(and (< b 1) (> e b)) NULL]
      [(< (expt b e)