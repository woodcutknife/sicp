(define (cube-root x)
  (define (cube-root-iter cur_root)
    (if (good-enough? cur_root)
      cur_root
      (cube-root-iter (improve-ans cur_root))))
  (define (good-enough? cur_root)
    (< (fabs (- (cube cur_root) x)) EPS))
  (define EPS 1e-6)
  (define (improve-ans cur_root)
    (/ (+ (/ x (square cur_root)) (* 2.0 cur_root)) 3.0))
  (cube-root-iter 1.0))

(define (fabs x)
  (if (< x 0)
    (- x)
    x))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))
