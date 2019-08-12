#lang sicp

; chapter 2 - Building Abstractions with Data

(define (linear-combination a b x y)
  (+ (* a b) (* x y)))

(define (linear-combination a b x y)
  (add (mul a x) (mul b y)))
