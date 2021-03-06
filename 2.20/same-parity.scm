(define (same-parity first-item . other-items)
  (define (parity item) 
    (even? item))
  (define first-item-parity (parity first-item))
  (define (recur items)
    (if (null? items)
      (list)
      (if (eq? first-item-parity (parity (car items)))
        (cons (car items) (recur (cdr items)))
        (recur (cdr items)))))
  (cons first-item (recur other-items)))
