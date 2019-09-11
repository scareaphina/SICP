#lang sicp

;SICP 2.2 heirarchical data and the closure property

(cons (cons 1 2)
      (cons 3 4))

(cons (cons 1
            (cons 2 3))
      4)

(cons 1
      (cons 2
            (cons 3
                  (cons 4 null))))

(list 1 2 3 4)

; In general,
;    (list <a1> <a2> ... <an>)
; is equivalent to
;    (cons <a1> (cons <a2> (cons ... (cons <an> nil) ...)))

(define one-through-four (list 1 2 3 4))

(car one-through-four)

(cdr one-through-four)
(car (cdr one-through-four))

(cons 10 one-through-four)

(cons 5 one-through-four)
