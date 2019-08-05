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
