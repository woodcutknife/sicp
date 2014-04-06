;; random decimal in range [0, 1)
(define (random-real)
  (let ((rand-max 4294967087))
    (/ (random rand-max)
       (exact->inexact rand-max))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* range (random-real)))))


(define (estimate-pi trials)
  (* 4.0
     (estimate-intergral (lambda (x y)
                           (<= (sqrt (+ (* x x) (* y y))) 1))
                         -1 1
                         -1 1
                         trials)))

(define (estimate-intergral P x1 x2 y1 y2 trials)
  (define (experiment)
    (P (random-in-range x1 x2)
       (random-in-range y1 y2)))
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
            (/ trials-passed trials))
           ((experiment)
            (iter (- trials-remaining 1) (+ trials-passed 1)))
           (else
            (iter (- trials-remaining 1) trials-passed))))
  (iter trials 0))
