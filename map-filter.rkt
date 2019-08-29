(define (map fn list)
  (if (empty? list)
      '()
      (se (fn (first list)) (map fn (bf list)))))

(define (filter pred list)
  (if (empty? list)
      '()
      (if (equal? (pred (first list)) #t)
          (se (first list) (filter pred (bf list)))
          (filter pred (bf list)))))