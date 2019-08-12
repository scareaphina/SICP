; 1.29
; had to look up because the math is complicated.

(define (round-to-next-even x)
  (+ x (remainder x 2)))

(define (simpson f a b n)
  (define fixed-n (round-to-next-even n))
  (define h (/ (- b a) fixed-n))
  (define (simpson-term k)
    (define y (f (+ a (* k h))))
    (if (or (= k 0) (= k fixed-n))
        (* 1 y)
        (if (even? k)
            (* 2 y)
            (* 4 y))))
  (* (/ h 3) (sum simpson-term 0 inc fixed-n)))

; 1.30

(define (sum f a next stopnumber)
  (define (iter a result)
    (if (> a stopnumber)
        result
        (iter (next a) (+ result (f a)))))
    (iter a 0))

; 1.31

(define (product f a next stopnumber)
  (if (> a stopnumber)
      1
      (* (f a)
         (product f (next a) next stopnumber))))

(define (product f a next stopnumber)
  (define (iter a result)
    (if (> a b) result
        (iter (next a) (* (f a) result))))
  (iter a 1))

; 1.32

;recursive process
(define (accumulate combiner null-value term a next stopnumber)
  (if (> a stopnumber) null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next stopnumber))))

; iterative process
(define (accumulate combiner null-value term a next stopnumber)
  (define (iter a result)
    (if (> a stopnumber) result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))

; defining sum and product as calls to accumulate
(define (sum term a next stopnumber)
  (accumulate + 0 term a next stopnumber))

(define (product term a next stopnumber)
  (accumulate * 1 term a next stopnumber))

; 1.34

(define (square x) (* x x))

(define (f g)
  (g 2))

(f square)

(f (lambda (z) (* z (+ z 1))))

(f f)

; cannot return f of itself

; 1.3.4
; taking a procedure and returning a procedure as an argument 

(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (> (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

((average-damp square) 10)

(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

; Newton's method

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (cube x) (* x x x))
((deriv cube) 5)

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

(define (sqrt x)
  (newtons-method (lambda (y) (- (square y) x))
                  1.0))

; abstractions and first-class procedures

(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))
