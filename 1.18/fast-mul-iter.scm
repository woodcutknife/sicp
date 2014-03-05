(define (fast-mul a b)
  (define (iter cur_b cur_n res)
    (cond ((= cur_n 0) res)
          ((is-odd? cur_n) (iter (double cur_b) (halve cur_n) (+ res cur_b)))
          (else            (iter (double cur_b) (halve cur_n) res))))
  (iter a b 0))

(define (halve x)
  (div x 2))

(define (double x)
  (+ x x))

(define (is-odd? x)
  (= (remainder x 2) 1))
