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
      
; mapping over lists

(define (scale-list1 items factor)
  (if (null? items)
      null
      (cons (* (car items) factor)
            (scale-list1 (cdr items) factor))))

(scale-list1 (list 1 2 3 4 5) 10)

(define (map proc items)
  (if (null? items)
      null
      (cons (proc (car items))
            (map proc (cdr items)))))

(map abs (list -10 2.5 -11.6 17))

(map (lambda (x) (* x x))
     (list 1 2 3 4))

(define (scale-list items factor)
  (map (lambda (x) (* x factor))
       items))

(scale-list (list 1 2 3 4 5) 10)      

; 2.21

(define (square n)
  (* n n))

(define (square-list items)
  (if (null? items)
      items
      (cons (square (car items))
            (square-list (cdr items)))))

(define (square-list2 items) 
  (map (lambda (x) (square x)) items))

(check-equal? (square-list '(1 2 3 4)) '(1 4 9 16))
(check-equal? (square-list2 '(1 2 3 4)) '(1 4 9 16))

; heirarchical structures

(cons (list 1 2) (list 3 4))

(define x (cons (list 1 2) (list 3 4)))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(length x)
(count-leaves x)

(list x x)

(length (list x x))

(count-leaves (list x x))

; mapping over trees

(define (scale-tree1 tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree1 (car tree) factor)
                    (scale-tree1 (cdr tree) factor)))))

(scale-tree1 (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10)

(define (scale-tree tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (scale-tree sub-tree factor)
             (* sub-tree factor)))
       tree))

; sequences as conventional interfaces

(define (square n)
  (* n n))

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))

(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
        null
        (let ((f (fib k)))
          (if (even? f)
              (cons f (next (+ k 1)))
              (next (+ k 1))))))
  (next 0))

; programs look different but are actually very similar

; enumerate: tree leaves --> filter: odd? --> map: square --> accumulate: +, 0
; enumerate: integers --> map: fib --> filter: even? --> accumulate: cons, ()

; the similarities are not evident because of the way that each program implements enumeration and spreads the distinct parts of the signal-flow throughout different parts of the individual programs











