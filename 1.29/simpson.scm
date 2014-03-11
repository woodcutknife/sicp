(define (sum term a next b)
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))

(define (simpson-intergral f a b n)
  (define h (/ (- b a) (* n 1.0)))
  (define (coef k) ; the coefficient of the k-th term
    (cond ((= k 0)    1)
          ((= k n)    1)
          ((even? k)  2)
          ((odd?  k)  4)
          (else       0)))
  (define (simpson-f k) ; the procedure that sum calls
    (* (coef k)
       (f (+ a (* k h)))))
  (* (/ h 3)
     (sum simpson-f 0 (lambda (x) (+ x 1)) n)))
