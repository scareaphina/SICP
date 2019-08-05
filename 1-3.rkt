;1.3.1

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-intigers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))
