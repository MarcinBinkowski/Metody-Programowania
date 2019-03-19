#lang racket

(define (repeated f n)
  (define (compose f g)
    (lambda (x)
      (f (g x))))
  (if (= n 0)
      identity
      (compose f (repeated f (- n 1)))))

(define (fixed-point f first-guess)
  (define precision 0.00001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) precision))
  (define (try guess)
    (let ((next(f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (average-damp f)
    (define (average x y)
      (/ (+ x y) 2))
    (lambda (x) (average x (f x))))


(define (nth-root x n)
  (define precision 0.00001)
  (define (floor-log2 x)
    (floor(log x 2)))
  (fixed-point ((repeated average-damp(floor-log2 n))  (lambda (y) (/ x (expt y (- n 1)))))1.0))


; testy dla funkcji nth-root

(define (tests)
  (define precision 0.00001)
  (define (distance x y)
    (abs (- x y)))
  (cond
    [(> (distance (nth-root (expt 2 2) 2) (expt 4 (/ 1 2))) precision) (print "1st test failed")]
    [(> (distance (nth-root (expt 14123434 2) 2) (expt(expt 14123434 2) (/ 1 2))) precision) (print "2nd test failed")]
    [(> (distance (nth-root 0 2) (expt 0 (/ 1 2))) precision) (print "3rd test failed")]
    [(> (distance (nth-root (expt 0.00000000000001 2) 2) (expt 0.00000000000001 (/ 1 2))) precision) (print "4th test failed")]
    [(> (distance (nth-root (expt 0.0012 15) 15) (expt (expt 0.0012 15) (/ 1 15))) precision) (print "5th test failed")]
    [(> (distance (nth-root 424323 1342) (expt 424323 (/ 1 1342))) precision) (print "7th test failed")]
    [(> (distance (nth-root 1 1) (expt 1 1)) precision) (print "8th test failed")]
    [(> (distance (nth-root 3 2) (expt 3 (/ 1 2))) precision) (print "10th test failed")]
    [else (print "all tests passed")]))


; Ekspermenty
(define (test-one-damp x n)
  (fixed-point (average-damp (lambda(y) (/ x (expt y (- n 1)))))1.0))

(define (test-two-damps x n)
  (fixed-point (average-damp(average-damp (lambda(y) (/ x (expt y (- n 1))))))1.0))

(define (test-three-damps x n)
  (fixed-point (average-damp(average-damp(average-damp (lambda(y) (/ x (expt y (- n 1)))))))1.0))

(define (test-four-damps x n)
  (fixed-point (average-damp(average-damp(average-damp(average-damp (lambda(y) (/ x (expt y (- n 1))))))))1.0))

(define (experiments)
  (define precision 0.00001)
  (define (distance x y)
    (abs (- x y)))
  (if (> (distance (test-one-damp (expt 2 2) 2) (expt 4 (/ 1 2))) precision) (print "1st test failed")  (pretty-write "1st test passed")) ;pretty-write adds \n after text
  (if (> (distance (test-one-damp (expt 2 3) 3) (expt (expt 2 3) (/ 1 3))) precision) (print "2nd test failed")  (pretty-write "2nd test passed"))

  ; Na tym tescie program nie kończy swojego działania
  ;(if (> (distance (test-one-damp (expt 2 4) 4) (expt (expt 2 4) (/ 1 4))) precision) (print "3rd test failed")  (pretty-write "3rd test passed")))
  (if (> (distance (test-two-damps (expt 2 4) 4) (expt (expt 2 4) (/ 1 4))) precision) (print "3rd test failed")  (pretty-write "3rd test passed"))
  (if (> (distance (test-two-damps (expt 2 4) 4) (expt (expt 2 3) (/ 1 3))) precision) (print "4th test failed")  (pretty-write "4th test passed"))
  (if (> (distance (test-two-damps (expt 2 5) 5) (expt (expt 2 3) (/ 1 3))) precision) (print "5th test failed")  (pretty-write "5th test passed"))
  (if (> (distance (test-two-damps (expt 2 6) 6) (expt (expt 2 3) (/ 1 3))) precision) (print "6th test failed")  (pretty-write "6th test passed"))
  (if (> (distance (test-two-damps (expt 2 7) 7) (expt (expt 2 3) (/ 1 3))) precision) (print "7th test failed")  (pretty-write "7th test passed"))

  ; Na tym tescie program nie kończy swojego działania
  ;(if (> (distance (test-two-damps (expt 2 8) 8) (expt (expt 2 3) (/ 1 3))) precision) (print "8th test failed")  (pretty-write "8th test passed"))
  (if (> (distance (test-three-damps (expt 2 8) 8) (expt (expt 2 3) (/ 1 3))) precision) (print "8th test failed")  (pretty-write "8th test passed"))
  (if (> (distance (test-three-damps (expt 2 9) 9) (expt (expt 2 3) (/ 1 3))) precision) (print "9th test failed")  (pretty-write "9th test passed"))

  ; wnioskujac po dotychczasowych wynikach mozna wyciągnąć wniosek, że ilość zastosowanych tłumień ma związek z kolejnymi potęgami dwójki aby potwierdzić tę tezę
  ; przetestujmy czy ilość potrzebnych tłumień zgadza się z kolenymi jej potęgami.

  ; dla tego testu 3 tłumienia działają
  (if (> (distance (test-three-damps (expt 2 15) 15) (expt (expt 2 15) (/ 1 15))) precision) (print "10th test failed")  (pretty-write "10th test passed"))
  
  ; dla tego testu 3 tłumienia już nie działają
  ;(if (> (distance (test-three-damps (expt 2 16) 16) (expt (expt 2 16) (/ 1 16))) precision) (print "11th test failed")  (pretty-write "11th test passed"))

  ; jednak dla 4 tłumień znowu test jest wykonany  pozytywnym wynikiem
  (if (> (distance (test-four-damps (expt 2 16) 16) (expt (expt 2 16) (/ 1 16))) precision) (print "11th test failed")  (pretty-write "11th test passed"))

  ; dla 31 także
  (if (> (distance (test-four-damps (expt 2 31) 31) (expt (expt 2 31) (/ 1 31))) precision) (print "12th test failed")  (pretty-write "12th test passed"))
  ; jednak dla 32 znowu trzeba zwiększyć ilość tłumień
  ; (if (> (distance (test-four-damps (expt 2 32) 32) (expt (expt 2 32) (/ 1 32))) precision) (print "13th test failed")  (pretty-write "13th test passed"))

  ; z eksperymentów wynika, że poszukiwany wzór to "podłoga z logarytmu o podstawie 2 z n gdzie n jest stopniem pierwiastka"
  )





