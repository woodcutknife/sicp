(define (deep-reverse items)
  (cond ((null? items)          (list))
        ((not (pair? items))    items)
        (else                   (append (deep-reverse (cdr items)) (list (deep-reverse (car items)))))))
