(define (sum-eq-s-triples n s)
  (map (lambda (pair) 
         (append pair (list (- s (car pair) (cadr pair))))) ; generate triple from pair
       (filter (lambda (pair)
                 (let ((i (car pair))
                       (j (cadr pair)))
                   (let ((k (- s i j)))
                     (and (not (= k i))
                          (not (= k j))
                          (not (= i j))
                          (> k 0)
                          (< k (+ n 1)))))) ; let k = s - i - j, then check if triple (i, j, k) is valid
               (flatmap (lambda (i)
                          (map (lambda (j)
                                 (list i j))
                               (gen-range 1 n)))
                        (gen-range 1 n))))) ; generate {(i, j) : 1 <= i <= n, 1 <= j <= n}

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define (accumulate op init seq)
  (if (null? seq)
    init
    (op (car seq)
        (accumulate op init (cdr seq)))))

(define (gen-range l r)
  (if (> l r)
    '()
    (cons l (gen-range (+ l 1) r))))
