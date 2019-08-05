;;;;;;;;;;;;;;;;;;;;;;;
; NOTE to future phina: In Racket, the last function is always what gets returned. Keep that in minnnnnnd.
;;;;;;;;;;;;;;;;;;;;;;;

;1.2

;;;;;;;;;;;;;;;;;;;;;;;

;1.2.1

(define (factorial n)
  (if (= n 1)
      1
      (* n (factorial (- n 1)))))

; shows visually what the above function does

;(factorial 6)
;(* 6 (factorial 5))
;(* 6 (* 5 (factorial 4)))
;(* 6 (* 5 (* 4 (factorial 3))))
;(* 6 (* 5 (* 4 (* 3 (factorial 2)))))
;(* 6 (* 5 (* 4 (* 3 (* 2 (factorial 1))))))
;(* 6 (* 5 (* 4 (* 3 (* 2 1)))))
;(* 6 (* 5 (* 4 (* 3 2))))
;(* 6 (* 5 (* 4 6)))
;(* 6 (* 5 24))
;(* 6 120)
;720

; define the factorial function with a variable of n - if n is equal to one, return one, otherwise multiply n by procedure of factorial n

(define (factorial n)
  (fact-iter 1 1 n))

; define factorial n as calling fact-iter with the variable of 1 1 n

(define (fact-iter product counter max-count)
  (if (> counter max-count)
      product
      (fact-iter (* counter product)
                 (+ counter 1)
                 max-count)))

; define fact-iter with variable of product, counter, and max-count
; if counter is greater than max-count, return product
; otherwise, run fact-iter with a first factor of the product of counter and product, then the sum of counter and 1, followed by max-count

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

; define the function of fib with variable n
; if n is zero, return zero. if one return one.
; else add fib (n - 1) to fib (n - 2)

;;;;;;;;;;;;;;;;;;;;;;;

;1.2.2

(define (fib n)
  (fib-iter 1 0 n))

; define the function of fib which takes one argument
; return fib-iter which takes three arguments

(define (fib-iter a b count)
  (if (= count 0)
      b
      (fib-iter (+ a b) a (- count 1))))

; define fib of n by calling fib-iter
; define fib-iter of a b plus a count
; if count is zero, return b
; else fib iter (a + b) a and subtract 1 from the total count

;;;;;;;;;;;;;;;;;;;;;;;

(define (count-change amount)
  (cc amount 5))

; define the function count-change with the argument of whatever the amount is that you want to count
; return the function of cc with the argument of amount and another variable (in this case 5

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denomination kinds-of-coins))
                     kinds-of-coins)))))

; define cc with the two arguments of amount and kinds-of-coins
; if the amount equals zero, return one
; or the amount is greater than zero and koc is zero, 0
; if neither of those are true, add cc and the amount
; subtract one from kinds of coins
; call cc and subtract from the amount the function first-denom and koc from koc

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

; define first denom koc
; first value is 1, and has a value of one
; second is 2, has a value of 5
; and so on

(count-change 100)
; returns 292

;;;;;;;;;;;;;;;;;;;;;;;

;1.2.4

(define (expt b n)
  (if (= n 0)
      1
      (* b (expt b (- n 1)))))

; define function expt
; if n = 0, return 1, otherwise execute function and return results

(define (expt b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

; define expt b n as returning the results of expt-iter b n 1
; define expt-iter with a variable of b, then counter, then product
; if the counter is 0, return product
; otherwise, run expt-iter of variable b, subtract one from the counter, then multiply b by the product

;;;;;;;;;;;;;;;;;;;;;;;

(* b (* b (* b (* b (* b ( * b (* b b)))))))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

; define fast-expt with 2 variables
; if n = 0, return 1
; otherwise, if n is even, square the result of fast-expt b times n divided by 2
; otherwise, multiply b by the results of fast-expt b times n - 1

(define (even? n)
  (= (remainder n 2) 0))
  
;;;;;;;;;;;;;;;;;;;;;;;

1.2.5

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

;;;;;;;;;;;;;;;;;;;;;;;

1.2.6

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
  
;;;;;;;;;;;;;;;;;;;;;;;