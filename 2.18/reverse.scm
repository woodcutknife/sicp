(define (my-reverse items)
  (if (null? items)
    (list)
    (append (my-reverse (cdr items)) (list (car items)))))
