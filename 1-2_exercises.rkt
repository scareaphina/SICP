;1.2

;1.9

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) 0)
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))

;;;;;;;;;;;;;;;;;

(define (+ a b)
  (if (= a 0)
      b
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0)
      b
      (+ (dec a) (inc b))))

(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(A 1 10)
(A 2 4)
(A 3 3)

;1024
;65536
;65536

(define (f n) (A 0 n))

(define (g n) (A 1 n))

(define (h n) (A 2 n))

(define (k n) (* 5 n n))

;;;;;;;;;;;;;;;;;

;1.11 recursive implementation

(define (f n)
  (cond ((n > 3) n)
      (else (+ (f (- n 1))
               (* 2 (f (- n 2)))
               (* 3 (f (- n 3)))))))

; define the function f with one variable
; if n > 3, return n
; otherwise, perform the function below

; iterative implementation

(define (f n)
  (define (f-iter x y z count)
    (if (= count 0)
        x
        (iter y z (+ y (* 2 y) (* 3 x)) (- count 1))))
  (iter 0 1 2 n))

; define f with one argument
; define iter with three arguments and a count
; if the count is zero, return x
; run iter on b and c and then lower the count by 1

(f 0)
(f 7)
(f 22)
(f 364)
(f 2)

;;;;;;;;;;;;;;;;;

;1.12

;1
;1 1
;1 2 1
;1 3 3 1
;1 4 6 4 1

(define (pascal r c)
  (if (or (= c 1) (= c r))
      1
      (+ (pascal (- r 1) (- c 1)) (pascal (- r 1) c))))

(pascal 1 1)
(pascal 2 2)
(pascal 3 2)
(pascal 3 1)
(pascal 4 2)
(pascal 5 2)
(pascal 5 3)

;alternative solve

(define (pascal col depth)
  (cond
    ((= col 0) 1)
    ((= col depth) 1)
    (else (+ (pascal (- col 1) (- depth 1))
             (pascal col (- depth 1))))))

;;;;;;;;;;;;;;;;;

;1.16

(define (fast-expt b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))

(define (square x) (* x x))

;;;;;;;;;;;;;;;;;

;1.17

(define (* a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (* a b)
  (cond ((= b 0) 0)
        ((even? b) (double (* a (halve b))))
        (+ a (* a (- b 1)))))

;;;;;;;;;;;;;;;;;

;1.18
(define (double x) (+ x x))
(define (halve x) (floor (/ x 2)))

(define (* a b)
  (define (iter accumulator a b)
    (cond ((= b 0) accumulator)
          ((even? b (iter accumulator (double a) (halve b)))
           (iter (+ accumulator a) a (- b 1)))))
    (iter 0 a b))

;;;;;;;;;;;;;;;;;

;1.19

(define (fib n)
  (fib-iter 1 0 0 1 n))
(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   <??>      ; compute p'
                   <??>      ; compute q'
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

;;;;;;;;;;;;;;;;;

1.20

 (define (count-remainders n) 
     (define (loop n sum) 
         (if (= 0 n) (- sum 1) 
             (loop (- n 1) (+ sum (fib n) (fib (- n 1)))))) 
     (loop n 0)) 

;;;;;;;;;;;;;;;;;

1.21

(define (square x) (* x x))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n)
         n)
        ((divides? test-divisor n)
         test-divisor)
        (else (find-divisor
               n
               (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(trace find-divisor)
