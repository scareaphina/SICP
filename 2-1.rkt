#lang sicp

; chapter 2 - Building Abstractions with Data

(define (linear-combination a b x y)
  (+ (* a b) (* x y)))

(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))

; 2.1 introduction to data abstraction

; 2.1.1 arithmetic operations for rational numbers

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mult-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

; pairs
; car returns the first element of a pair, cdr returns the second

(define x (cons 1 2))

(car x)

(cdr x)

; cons can be used to form pairs whose elements are pairs

(define x (cons 1 2))
(define y (cons 3 4))
(define z (cons x y))

(car (car z))
(car (cdr z))

; data objects constructed from pairs are called list-structured data

; representing rational numbers

(define (make-rat n d) (cons n d))
(define (numer x) (car x))
(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(print-rat one-half)

(define one-third (make-rat 1 3))
(print-rat (add-rat one-half one-third))

(print-rat (mul-rat one-half one-third))

(print-rat (add-rat one-third one-third))

; the result to the last print-rat shows us that the procedure does not reduce rational numbers to lowest terms. we can fix that by changing our make-rat function

(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

; now, when we run

(print-rat (add-rat one-third one-third))

; we get the result

; 2/3

; 2.1.2 abstraction barriers
