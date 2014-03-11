(define (smooth f)
  (define dx 1e-6)
  (lambda (x)
    (/ (+ (f (- x dx)) (f x) (f (+ x dx)))
       3.0)))

(define (repeated f n)
  (if (= n 1)
    f
    (lambda (x)
      (f ((repeated f (- n 1)) x)))))

(define (smooth-n f n)
  ((repeated smooth n) f))
