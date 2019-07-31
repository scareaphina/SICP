;1.1.1

(+ 137 349)
(- 1000 334)
(* 5 99)
(/ 10 5)
(+ 2.7 10)

(+ 21 35 12 7)

(* 25 4 12)

(+ (* 3 5) (- 10 6))

(+ (* 3 (+ (* 2 4) (+ 3 5))) (+ (- 10 7) 6))

(+ (* 3
      (+ (* 2 4)
         (+ 3 5)))
   (+ (- 10 7)
      6))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;1.1.2

(define size 2)
size
( * 5 size)

(define pi 3.14159)
(define radius 10)
(* pi (* radius radius))
(define circumference (* 2 pi radius))
circumference

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;1.1.3

(* (+ 2  (* 4 6))
   (+ 3 5 7))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;1.1.4

(define (square x) (* x x))

(square 21)

(square (+ 2 5))

(square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;1.1.5

(define (square x) (* x x))

(square 21)

(square (+ 2 5))

(square (square 3))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(define (f a)
  (sum-of-squares (+ 5 1) (* 5 2)))

(f 5)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;1.1.6

(define (abs x)
  (cond ((> x 0) x)
        ((= x 0) 0)
        ((< x 0) (- x))))

(define (abs x)
  (cond ((< x 0) (- x))
        (else x)))

(define (abs x)
  (if (< x 0)
      (- x)
      x))

10
(+ 5 3 4)
(- 9 1)
(/ 6 2)
(+ (* 2 4) (- 4 6))
(define a 3)
(define b (+ a 1))
(+ a b (* a b))
(= a b)
(if (and (> b a) (< b (* a b)))
    b
    a)
(cond ((= a 4) 6)
      ((= b 4) (+ 6 7 a))
      (else 25))
(+ 2 (if (> b a) b a))
(* (cond ((> a b) a)
         ((< a b) b)
         (else -1))
   (+ a 1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;1.1.7

(define (square x) (* x x))
(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))
(define (improve guess x)
  (average guess (/ x guess)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

; square x is equal to x times itself
; average x y is equal to x + y / 2
; sqrt calls sqrt-iter
; sqrt-iter says guess variable x
; if that's good enough, guess x and return guess
; if not, call sqrt-iter again and improve the guess of x, then return x, repeating as necessary to refine
; improve takes guess and x as variables, then returns the average guess by dividing x by the guess
; good enough takes guess and x and returns if the result is greater than the absolute value of the square guess minus x within a value of 0.001

(sqrt 9)

(sqrt (+ 100 37))

(sqrt (+ (sqrt 2) (sqrt 3)))

(square (sqrt 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;

1.1.8

(define (square x) (* x x))

(define (square x)
  (exp (double (log x))))

(define (double x) (+ x x))

; these processes all do the same thing but with different details. and example of how procedure definition supresses detail. they are black box procedures that can be called to do the same functions

; ****** if the parameters were not local to the bodies of their respective procedures, then the parameter x in square could be confused with the parameter x in good-enough?, and the behavior of good-enough? would depend upon which version of square we used. Thus, square would not be the black box we desired. *********

(define (sqrt x)
  (sqrt-iter 1.0 x))
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x) x)))
(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))

; if you have a larger system, this becomes a problem because functions like good-enough or improve might be needed elsewhere

(define (sqrt x)
  (define (good-enough? guess x)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess x)
    (average guess (/ x guess)))
  (define (sqrt-iter guess x)
    (if (good-enough? guess x)
        guess
        (sqrt-iter (improve guess x) x)))
  (sqrt-iter 1.0 x))

; block structure - but we can not only internalize the definitions of auxiliary procedures, we can simplify them
; since x is bound in the definition of sqrt, good-enough?, improve, and sqrt-iter, are in the scope of x
; so we do not need to pass x ecplicitly to each procedure, we can just let x be a free variable in the internal definitions

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
