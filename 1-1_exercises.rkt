;1.1

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

;;;;;;;;;;;;;;;;;;;;;;;;;

;1.2
(/ (+
    (+ 5 4)
    (- 2
       (- 3
          (+ 6
             (/ 4 5)))))
   (* 3
      (- 6 2)
      (- 2 7)))
-37/150

;;;;

;second attempt
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5)))))
   (* 3 (* (- 6 2) (- 2 7))))

;;;;;;;;;;;;;;;;;;;;;;;;;

;1.3

(define (square x) (* x x))
(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sumsqlgst a b c)
  (cond
    ((and (>= a c) (>= b c)) (sum-of-squares a b))
    ((and (>= b a) (>= c a)) (sum-of-squares b c))
    ((and (>= a b) (>= c b)) (sum-of-squares a c))))

(square-largest 2 5 9)

(square-largest 17 82 5)

(square-largest .75 8 2)

; second attempt

(define (square x) (* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

(define (sumsq-largest a b c)
  (cond ((> a b) (> b c)
        (+ (sum-of-squares a b)))
        ((> a c) (> c b)
        (+ (sum-of-squares a c)))
        (+ (sum-of-squares b c))))

(sumsq-largest 1 2 3)
(sumsq-largest 5 1 10)
(sumsq-largest 2 10 1)

; third attempt

(define (square x)
  (* x x))

(define (sum-of-squares x y)
  (+ (* x x) (* y y)))

(define (sum-greater x y z)
  (cond
    ((and (> x y) (> y z)) (sum-of-squares x y))
    ((and (> x z) (> z y)) (sum-of-squares x z))
    ((and (> z x) (> y x)) (sum-of-squares z y))))

(sum-greater 2 5 9)

(sum-greater 17 82 5)

(sum-greater .75 8 2)

;;;;;;;;;;;;;;;;;;;;;;;;;

; 1.4.  Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:

(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))

; define procedure a-plus-abs-a-b
; if b is greater than zero plus or minus one, return a b

;;;;;;;;;;;;;;;;;;;;;;;;;

;1.8

(define (square x)
  (* x x))

(define (cube x)
  (* (square x) x))

(define (good-enough? guess prior-guess)
  (< (abs (- (square guess) (square prior-guess))) 0.001))

(define (improve-cube guess x)
  (/ (+ (* 2 guess) (/ x (square guess))) 3))

(define (cubrt-iter guess prior-guess x)
  (if (good-enough? guess prior-guess)
      guess
      (cubrt-iter (improve-cube guess x)
                  guess
                 x)))
(define (cube-root x)
  (cubrt-iter 1.0 0.0 x))

(cube-root 27)
(cube-root 382)
(cube-root .73)
