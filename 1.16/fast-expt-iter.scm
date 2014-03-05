(define (fast-expt b n)
  (define (iter cur_b cur_n res)
    (cond ((= cur_n 0) res)
          ((is-odd? cur_n) (iter (square cur_b) (div cur_n 2) (* res cur_b)))
          (else            (iter (square cur_b) (div cur_n 2) res))))
  (iter b n 1))

(define (is-odd? n)
  (= (remainder n 2) 1))

(define (square x)
  (* x x))
