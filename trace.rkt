#lang sicp

(define (trace a st)
  (display st)
  (display a)
  (display "\n")
  a)

(define (filter predicate sequence)
  (cond ((null? sequence) null)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))
