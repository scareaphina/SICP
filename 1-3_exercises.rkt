; 1.29


; 1.30

(define (sum f a next stopnumber)
  (define (iter a result)
    (if (> a stopnumber)
        result
        (iter (next a) (+ result (f a)))))
    (iter a 0))
