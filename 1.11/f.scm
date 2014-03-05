(define (f-recur n)
  (if (< n 3)
    n
    (+ (f-recur (- n 1))
       (* 2 (f-recur (- n 2)))
       (* 3 (f-recur (- n 3))))))

(define (f-iter n)
  (define (iter a b c count)
    (if (= count n)
      a
      (iter b c (+ c (* 2 b) (* 3 a)) (+ count 1))))
  (iter 0 1 2 0))
