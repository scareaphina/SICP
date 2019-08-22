(define (assert-equal x y)
  (if (equal? x y)
      #t
      (string-append "failed: expected " (symbol->string y) " was "  (symbol->string x))))

(define (tests a . b)
  (display (foldl (lambda (x acum)
                    (if (equal? #t x)
                        (string-append acum ".")
                        (string-append acum  x " F\n")))
                  ""
                  (cons a b))))
