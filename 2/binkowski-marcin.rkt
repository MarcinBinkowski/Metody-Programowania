#lang racket
;Współpraca Łukasz Jagnow, Natalia Turek, Marcin Binkowski

(define (continued-fraction num den)
  (define precision 0.00001)
  (define (good-enough? old-guess guess)
    (< (abs (- old-guess guess)) precision))
  (define(next-a prev-a a i)
    (+ (* (den i) a) (* (num i) prev-a)))
  (define(next-b prev-b b i)
    (+ (* (den i) b) (* (num i) prev-b)))
  (define (fraction-iter prev-a prev-b a b i)
    (if (good-enough? (/ a b) (/ (next-a prev-a a i) (next-b prev-b b i)))
        (/ a b)
        (fraction-iter a b (next-a prev-a a (+ i 1)) (next-b prev-b b (+ i 1)) (+ i 1))))
  (fraction-iter 1 0 0 1 1))


; Tests

(define (arctang x)
  (define (atan-num x)
      (lambda (i)
        (if (= i 1)
            x
            (expt (* (- i 1) x) 2))))
  (define (atan-den i)
      (- (* 2 i) 1))
  (/ x (+ 1 (continued-fraction (atan-num x) atan-den))))

(define (tests)
  (define (distance x y)
    (abs(- x y)))
  (define precision 0.00001)
  (cond
    [(> (distance (arctang 4) (atan 4)) precision) (print "test failed on calculating arctang 4")]
    [(> (distance (arctang 0.1) (atan 0.1)) precision) (print "test failed on calculating arctang 0.1")]
    [(> (distance (arctang 0.3) (atan 0.3)) precision) (print "test failed on calculating arctang 0.3")]
    [(> (distance (arctang 0) (atan 0)) precision) (print "test failed on calculating arctang 0")]
    [(> (distance (arctang 20) (atan 20)) precision) (print "test failed on calculating arctang 20")]
    [else (print "all tests passed")]))

