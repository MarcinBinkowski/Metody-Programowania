#lang Racket

(define (distance x y)
  (abs (- x y)))


(define (cube-root x)
  (define (square x)
    (* x x))
  (define (cube x) (* x x x))
    (define (better-approx approx)
    (iter (/ ( + ( / x (square approx)) (* 2 approx))3)))
  (define (iter approx)
    (if (> x 0)
    (cond
      [(> (distance x (cube approx)) 0.0001) (better-approx approx)]
      [else approx])
    (cond
      [(> (distance x (cube approx)) 0.0001) (better-approx (- approx))]
      [else  approx]))
    )
    (iter 1.0)  

    )
  
(cond
  [(> (distance (cube-root 10) (expt 10 1/3) ) 0.0001) (print "test failed")]
  [(> (distance (cube-root 20) (expt 20 1/3) ) 0.0001) (print "test failed")]
  [(> (distance (cube-root 2) (expt 2 1/3) ) 0.0001) (print "test failed")]
  [(> (distance (cube-root 3) (expt 3 1/3) ) 0.0001) (print "test failed")]
  [(> (distance (cube-root 1) (expt 1 1/3) ) 0.0001) (print "test failed")]
  [(> (distance (cube-root 17) (expt 17 1/3) ) 0.0001) (print "test failed")]
  [(> (distance (cube-root 18) (expt 18 1/3) ) 0.0001) (print "test failed")]
  [else (print "all passed")])
