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

; list operations

(define (list-ref items n)
  (if (= n 0)
      (car items)
      (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(list-ref squares 3)

(define (length items)
  (if (null? items)
      0
      (+ 1 (length (cdr items)))))

(define odds (list 1 3 5 7))

(length odds)

(define (lengthiter items)
  (define (length-iter a count)
    (if (null? a)
        count
        (length-iter (cdr a) (+ 1 count))))
  (length-iter items 0))

(append squares odds)

(append odds squares)

(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))
      
      
      
      
      
      
      
      
      
