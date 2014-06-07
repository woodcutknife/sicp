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

(define-class <cat> <object> size breed)

(define garfield (make <cat> (size 6) (breed 'weird)))

(breed garfield)

(define-method 4-legged? ((x <cat>)) true)

(define-method 4-legged? ((x <object>)) 'Who-knows?)

(4-legged? garfield)

(4-legged? 'Hal)

(define-method say ((cat <cat>) (stuff <object>))
  (print 'meow)
  (print stuff))
  
(define-method say ((cat <cat>) (number <number>))
  (print 'i-do-not-recognize-numbers)
  (print 'oooooooooooooooooooooooops))
  
(say garfield '(feed me))

(say garfield 563)

(define-class <house-cat> <cat> address)

(define fluffy (make <house-cat> (size 'tiny) (address 'America)))

(breed fluffy)

(size fluffy)

(address fluffy)

(say fluffy '(feed fluffy))

(define-method say ((cat <house-cat>) (stuff <object>))
  (print 'i-am-a-house-cat)
  (print stuff)
  (print 'i-am-specail!))

(say garfield 'pardon?)

(say fluffy 'pardon?)

(say fluffy 443)

(define-method shout ((stuff <object>) (cat <house-cat>))
  (print 'shout!)
  (print stuff))
  
(define-method shout ((number <number>) (cat <cat>))
  (print 'shout-number!)
  (print number))
  
(shout 43 fluffy)

(shout 'you-are-great! fluffy)
