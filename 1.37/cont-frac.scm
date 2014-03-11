(define (cont-frac-iter n d k)
  (define (iter n d k res)
    (if (= k 0)
      res
      (iter n d (- k 1) (/ (n k) (+ (d k) res)))))
  (iter n d k 0))

(define (cont-frac-recur n d k)
  (define (recur n d step)
    (if (= step k)
      (/ (n k) (d k))
      (/ (n k) (+ (d k) (recur n d (+ step 1))))))
  (recur n d 1))

(cont-frac-iter  (lambda (i) 1.0) (lambda (i) 1.0) 12)
(cont-frac-recur (lambda (i) 1.0) (lambda (i) 1.0) 12)
