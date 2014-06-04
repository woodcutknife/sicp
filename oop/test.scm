;;; Tutorial exercise 2

(define-class <vector> <object> xcor ycor)

(define-method + ((v1 <vector>) (v2 <vector>))
               (make <vector> (xcor (+ (get-slot v1 'xcor)
                                       (get-slot v2 'xcor)))
                     (ycor (+ (get-slot v1 'ycor)
                              (get-slot v2 'ycor)))))

(define-method * ((v1 <vector>) (v2 <vector>))
               (+ (* (get-slot v1 'xcor) 
                     (get-slot v2 'xcor)) 
                  (* (get-slot v1 'ycor) 
                     (get-slot v2 'ycor))))

(define-method * ((v <vector>) (n <number>))
               (make <vector> (xcor (* (get-slot v 'xcor)
                                       n))
                     (ycor (* (get-slot v 'ycor)
                              n))))

(define-method * ((n <number>) (v <vector>))
               (make <vector> (xcor (* n
                                       (get-slot v 'xcor)))
                     (ycor (* n
                              (get-slot v 'ycor)))))

(define-generic-function length)

(define-method length ((o <object>))
               (sqrt (* o o)))


;;; Tutorial exercise 5

(define-method print ((v <vector>))
               (print (cons (get-slot v 'xcor)
                            (get-slot v 'ycor))))


;;; Lab exercise 9

(define-class <cat> <object> size breed)

(define garfield (make <cat> (size 6) (breed 'weird)))

(print (breed garfield))

(define-method 4-legged? ((x <cat>)) true)

(define-method 4-legged? ((x <object>)) 'Who-knows?)

(print (4-legged? garfield))

(print (4-legged? 'Hal))

(define-method say ((cat <cat>) (stuff <object>))
               (print 'meow)
               (print stuff))

(define-class <house-cat> <cat> address)

(define fluffy (make <house-cat> (size 'tiny)))

(print (breed fluffy))

(say garfield '(feed me))

(say fluffy '(feed me))

(define-class <3dvector> <vector> zcor)

(define hv (make <3dvector> (xcor 1) (ycor 2) (zcor 3)))

(print (zcor hv))

(print (xcor hv))


;;; News

(define-class <vector> <object> xcor ycor)

(define-method print ((v <vector>))
  (print (cons (xcor v) (ycor v))))

(define-method + ((v1 <vector>) (v2 <vector>))
  (make <vector>
        (xcor (+ (xcor v1) (xcor v2)))
        (ycor (+ (ycor v1) (ycor v2)))))

(define v1 
  (make <vector>
        (xcor (make <vector> (xcor 1) (ycor 5)))
        (ycor 4)))

(define v2
  (make <vector>
        (xcor (make <vector> (xcor -2) (ycor 2)))
        (ycor -1)))

(+ v1 v2)

(ycor v2)

(xcor v1)

(define-class <3d-vector> <vector> zcor)

(define v3
  (make <3d-vector>
        (xcor (make <vector> (xcor -1) (ycor 3)))
        (ycor 2)
        (zcor -3)))

(zcor v3)

(xcor v3)

(+ v1 v3)

(define-method + ((v1 <vector>) (v2 <3d-vector>))
  (make <3d-vector>
        (xcor (+ (xcor v1) (xcor v2)))
        (ycor (+ (ycor v1) (ycor v2)))
        (zcor (+ (zcor v2) 100))))

(+ v1 v3)

(define-method print ((v <3d-vector>))
  (print (cons (xcor v) (cons (ycor v) (zcor v)))))

(+ v1 v3)
