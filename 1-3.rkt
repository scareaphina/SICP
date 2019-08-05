;1.3.1

(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-intigers (+ a 1) b))))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-cubes (+ a 1) b))))

(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

;;;

(define (inc n) (+ n 1))
(define (sum-cubes a b)
  (sum-cube a inc b))

(sum-cubes 1 10)

;;;

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc bb))

(sum-integers 1 10)

(define (sum f a next stopnumber)
  (if (> a stopnumber)
      0
      (+ (f a)
         (sum f (next a) next stopnumber))))
;;; renaming variables so that they are easier to understand in terms of the actual function they perform

;;;

(define (cube x) (* x x x))

(define (pi-sum a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum 1 1000))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

(integral cube 0 1 0.01)
(integral cube 0 1 0.001)

; 1.3.2

(lambda (x) (+ x 4))

(lambda (x) (/ 1.0 (* x (+ x 2))))

(define (pi-sum a b)
  (sum (lambda (x) (/ 1.0 (* x (+ x 2))))
       a
       (lambda (x) (+ x 4))
       b))

