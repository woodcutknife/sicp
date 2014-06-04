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
