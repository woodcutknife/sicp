(define (filtered-accumulate combiner custom-filter null-value term a next b)
  (define this-term (term a))
  (define (suffix-terms-value) (filtered-accumulate combiner custom-filter null-value term (next a) next b))
  (cond ((> a b) null-value)
        ((not (custom-filter this-term)) (suffix-terms-value))
        (else (combiner this-term
                        (suffix-terms-value)))))

(define (sum-of-primes-in-range a b)
  (filtered-accumulate + prime? 0 (lambda (x) x) a inc b))
(define (prime? n)
  (define (prime?-iter n k)
    (if (> (sqr k) n)
      #t
      (and (not (= (remainder n k) 0))
           (prime?-iter n (inc k)))))
  (if (< n 2)
    #f
    (prime?-iter n 2)))

(define (sum-of-numbers-that-coprime-with-n n)
  (define (coprime-with-n? x)
    (coprime? x n))
  (filtered-accumulate * coprime-with-n? 1 (lambda (x) x) 1 inc (- n 1)))
(define (coprime? a b)
  (define (my-gcd a b)
    (if (= b 0)
      a
      (my-gcd b (remainder a b))))
  (= 1 (my-gcd a b)))

(define (inc n)
  (+ n 1))
(define (sqr x)
  (* x x))
