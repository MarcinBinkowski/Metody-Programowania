#lang racket

;4
(struct document (title author chapters))
(struct chapter (title hs))
(struct par(text))
(define (document? d)
  (match d
    [(document t  a ch)
     (and (string? t)
          (string? a)
          (list? ch)
          (andmap chapter? ch))]))
(define (chapter? ch)
  (math ch
        [(chapter t hs)
         (and (string? t)
              (list? hs)
              (andmap
               (lambda (h)
                 (or (chapter? h)
                     (par? h)))hs))]))
(define(par? q)
  (match q
    [(par p)
     (and (list? p)
          (>= (length p) 1)
          (andmap string? p))]
    [else #f]))

;5

(define (text->html doc)
  (string-append "<html><head><title"
                 (document-title doc)
                 "</title><author>"
                 (doc author doc)
                 "</author></head><body>"
                 (chapter->html (doc-chapters doc)1)))

(define(chapter->html ch n)
  (match ch
    [(chapter title elems)
     (string-append
      "<h"
      (number->string n)
      ">"
      (apply string-append (map f elems))
      "</h"
      (number->string n) ">")]))