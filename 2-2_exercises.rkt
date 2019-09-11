#lang sicp

; 2.17

(define (last-pair list)
  (if (null? list)
      0
      (last list)))

(check-equal? (last-pair '(list 23 72 149 34)) '34)

(last-pair '(list 23 72 149 34))
