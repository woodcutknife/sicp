;;; logical functions

(define (logical-or a b)
  (if (or (= a 1) (= b 1))
    1
    0))

(define (logical-and a b)
  (if (and (= a 1) (= b 1))
    1
    0))

(define (logical-not a)
  (if (= a 0)
    1
    0))


;;; wire

(define (make-wire)
  (let ((signal-value 0) (action-procedures '()))
    (define (set-my-signal! new-value)
      (if (not (= signal-value new-value))
        (begin (set! signal-value new-value)
               (call-each action-procedures))
        'done))

    (define (accept-action-procedure! proc)
      (set! action-procedures (cons proc action-procedures))
      (proc))

    (define (dispatch m)
      (cond ((eq? m 'get-signal) signal-value)
            ((eq? m 'set-signal!) set-my-signal!)
            ((eq? m 'add-action!) accept-action-procedure!)
            (else (error "Unknown operation -- WIRE" m))))
    dispatch))

(define (call-each procedures)
  (if (null? procedures)
    'done
    (begin
      ((car procedures))
      (call-each (cdr procedures)))))

(define (get-signal wire)
  (wire 'get-signal))

(define (set-signal! wire new-value)
  ((wire 'set-signal!) new-value))

(define (add-action! wire action-procedure)
  ((wire 'add-action!) action-procedure))


;;; agenda

(define (make-time-segment time queue)
  (cons time queue))

(define (segment-time s) (car s))

(define (segment-queue s) (cdr s))

(define (make-agenda) (list 0))

(define (current-time agenda) (car agenda))

(define (set-curent-time! agenda time)
  (set-car! agenda time))

(define (segments agenda) (cdr agenda))

(define (set-segments! agenda segments)
  (set-cdr! agenda segments))

(define (first-segment agenda) (car (segments agenda)))

(define (rest-segments agenda) (cdr (segments agenda)))

(define (empty-agenda? agenda)
  (null? (segments agenda)))

(define (add-to-agenda! time action agenda)
  (define (belongs-before? segments)
    (or (null? segments)
        (< time (segment-time (car segments)))))
  (define (make-new-time-segment time action)
    (let ((q (make-queue)))
      (insert-queue! q action)
      (make-time-segment time q)))
  (define (add-to-segments! segments)
    (if (= (segment-time (car segments)) time)
      (insert-queue! (segment-queue (car segments))
                     action)
      (let ((rest (cdr segments)))
        (if (belongs-before? rest)
          (set-cdr!
            segments
            (cons (make-new-time-segment time action)
                  (cdr segments)))
          (add-to-segments! rest)))))
  (let ((segments (segments agenda)))
    (if (belongs-before? segments)
      (set-segments!
        agenda
        (cons (make-new-time-segment time action)
              segments))
      (add-to-segments! segments))))

(define (remove-first-agenda-item! agenda)
  (let ((q (segment-queue (first-segment agenda))))
     (delete-queue! q)
     (if (empty-queue? q)
       (set-segments! agenda (rest-segments agenda)))))

(define (first-agenda-item agenda)
  (if (empty-agenda? agenda)
    (error "Agenda is empty -- FIRST-AGENDA-ITEM")
    (let ((first-seg (first-segment agenda)))
      (set-curent-time! agenda (segment-time first-seg))
      (front-queue (segment-queue first-seg)))))


(define (after-delay delay action)
  (add-to-agenda! (+ delay (current-time the-agenda))
                  action
                  the-agenda))

(define (propagate)
  (if (empty-agenda? the-agenda)
    'done
    (let ((first-item (first-agenda-item the-agenda)))
      (first-item)
      (remove-first-agenda-item! the-agenda)
      (propagate))))

;;; queue

(define (front-ptr queue) (car queue))

(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item) (set-car! queue item))

(define (set-rear-ptr! queue item) (set-cdr! queue item))

(define (empty-queue? queue) (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue (cdr (front-ptr queue)))
         queue)))

;;; gates

(define (identity input output)
  (define (identity-input)
    (let ((new-value (get-signal input)))
      (after-delay identity-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input identity-input)
  'ok)

(define (inverter input output)
  (define (invert-input)
    (let ((new-value (logical-not (get-signal input))))
      (after-delay inverter-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! input invert-input)
  'ok)

(define (and-gate a1 a2 output)
  (define (and-action-procedure)
    (let ((new-value
            (logical-and (get-signal a1) (get-signal a2))))
      (after-delay and-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! a1 and-action-procedure)
  (add-action! a2 and-action-procedure)
  'ok)

(define (or-gate o1 o2 output)
  (define (or-action-procedure)
    (let ((new-value
            (logical-or (get-signal o1) (get-signal o2))))
      (after-delay or-gate-delay
                   (lambda ()
                     (set-signal! output new-value)))))
  (add-action! o1 or-action-procedure)
  (add-action! o2 or-action-procedure)
  'ok)


;;; global definitions

(define (probe name wire)
  (add-action! wire
               (lambda ()
                 (display name)
                 (display " ")
                 (display (current-time the-agenda))
                 (display "  New-value = ")
                 (display (get-signal wire))
                 (newline))))


(define the-agenda (make-agenda))
(define identity-delay 1)
(define inverter-delay 2)
(define and-gate-delay 3)
(define or-gate-delay 5)


;;; test

; adder - take two lists of wires as input, output the sum of the two numbers

(define (half-adder a b s c)
  (let ((d (make-wire)) (e (make-wire)))
    (or-gate a b d)
    (and-gate a b c)
    (inverter c e)
    (and-gate d e s)
    'ok))

(define (full-adder a b c-in sum c-out)
  (let ((s (make-wire))
        (c1 (make-wire))
        (c2 (make-wire)))
    (half-adder b c-in s c1)
    (half-adder a s sum c2)
    (or-gate c1 c2 c-out)
    'ok))

(define (adder input-1 input-2 output)
  (define (iter in-1 in-2 out last-c)
    (if (null? in-1)
      'ok
      (let ((next-c (make-wire)))
        (if (not (null? last-c))
          (full-adder (car in-1)
                      (car in-2)
                      last-c
                      (car out)
                      next-c)
          (half-adder (car in-1)
                      (car in-2)
                      (car out)
                      next-c))
        (iter (cdr in-1) (cdr in-2) (cdr out) next-c))))
  (let ((sz (length input-1)))
    (if (or (not (= sz (length input-2)))
            (not (= sz (length output))))
      (error "ADDER called with different amount of input/output wires")
      (iter input-1 input-2 output '()))))

(define (probe-list names wires)
  (if (not (null? names))
    (begin (probe (car names) (car wires))
           (probe-list (cdr names) (cdr wires)))))


(define input-1 (list (make-wire) (make-wire) (make-wire) (make-wire)))
(define input-2 (list (make-wire) (make-wire) (make-wire) (make-wire)))
(define output (list (make-wire) (make-wire) (make-wire) (make-wire)))
(probe-list '(d0 d1 d2 d3) output)
(adder input-1 input-2 output)
(set-signal! (car input-1) 1)
(set-signal! (car input-2) 1)
(propagate)
(set-signal! (car input-2) 0)
(set-signal! (cadr input-2) 1)
(set-signal! (caddr input-1) 1)
(propagate)
(set-signal! (cadr input-1) 1)
(propagate)
(map (lambda (w) (set-signal! w 1)) input-1)
(map (lambda (w) (set-signal! w 0)) input-2)
(propagate)
(set-signal! (car input-2) 1)
(propagate)


; parity - take a list of wires as input, output the parity of the count of wires whose signal is 1

(define (xor-gate x1 x2 output)
  (let ((n-x1 (make-wire)) (n-x2 (make-wire)) (a (make-wire)) (b (make-wire)))
    (inverter x1 n-x1)
    (and-gate n-x1 x2 a)
    (inverter x2 n-x2)
    (and-gate x1 n-x2 b)
    (or-gate a b output)
    'ok))

(define (parity inputs output)
  (define (iter in last)
    (if (null? in)
      (begin (identity last output)
             'ok)
      (let ((next (make-wire)))
        (if (not (null? last))
          (xor-gate last (car in) next)
          (identity (car in) next))
        (iter (cdr in) next))))
  (iter inputs '()))


(define inputs (list (make-wire) (make-wire) (make-wire) (make-wire) (make-wire)))
(define output (make-wire))
(probe 'parity output)
(parity inputs output)
(set-signal! (car inputs) 1)
(propagate)
(set-signal! (cadddr inputs) 1)
(propagate)
(set-signal! (car inputs) 1)
(set-signal! (cadr inputs) 1)
(propagate)
(set-signal! (car (cddddr inputs)) 1)
(set-signal! (cadr inputs) 0)
(propagate)
