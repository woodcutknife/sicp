(define (gen-range l r)
  (if (> l r)
    '()
    (cons l
          (gen-range (+ l 1)
                     r))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op init seq)
  (if (null? seq)
    init
    (op (car seq)
        (accumulate op init (cdr seq)))))

(define (for . lists) ; if there are n lists, procedure `for` will return a list of n-tuples, iterating i-th dimension with items in i-th list
  (define (my-for lists)
    (if (null? lists)
      (list (list))
      (flatmap (lambda (i)
                 (map (lambda (l)
                        (cons i l))
                      (my-for (cdr lists))))
               (car lists))))
  (my-for lists))

(define (sum-eq-s-triples n s)
  (filter (lambda (triple)
            (let ((i (car triple))
                  (j (cadr triple))
                  (k (caddr triple)))
              (and (not (= i j))
                   (not (= j k))
                   (not (= k i))
                   (= s
                      (+ i j k)))))
          (for (gen-range 1 n)
               (gen-range 1 n)
               (gen-range 1 n))))
