#lang racket

(require web-server/servlet
         web-server/servlet-env)

;; Zdefiniuj swój moduł w pliku text.rkt
;; Powinien on dostarczać (przy użyciu formy provide)
;; procedury (show-start-page) generującą stronę startową
;; i (show-prime-divs n) generującą stronę z dokumentem
;; pokazującym rozkład n na czynniki pierwsze
(require "text.rkt")

(define (respond-string s)
  (response/output (lambda (op)
                     (write-bytes (string->bytes/utf-8 s) op))))

(define-values (dispatcher urler)
  (dispatch-rules
   [("primedivs" (integer-arg)) prime-divs]
   [else hello-world]))

(define (prime-divs req n)
  (respond-string (show-prime-divs n)))

(define (hello-world req)
  (respond-string (show-start-page)))

(serve/servlet dispatcher #:servlet-path "/main" #:servlet-regexp (regexp ""))