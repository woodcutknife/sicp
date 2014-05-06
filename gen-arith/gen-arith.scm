;;; Code for creating the table, you don't need to worry about this.

(define (make-table)
  (let ((local-table (list '*table*)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)

    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))

    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))


;;; The bottom level typing system

(define attach-tag cons)

(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad typed datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
    (cdr datum)
    (error "Bad typed datum -- CONTENTS" datum)))


;;; The apply-generic mechanism.  
;;;  Note that we don't deal with coercion here.

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for the given types -- APPLY-GENERIC"
               (list op type-tags))))))


;;; GENERIC ARITHMETIC OPERATIONS

;;;   GN = ({number} X RepNum) U ({rational} X RepRat) U ({polynomial} X RepPoly)

;;;   (GN, GN) --> GN
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))


;;;   GN --> GN
(define (negate x) (apply-generic 'negate x))

;;;   GN --> Bool
(define (=zero? x) (apply-generic '=zero? x))

;;;   (GN, GN) --> Bool
(define (equ? x y) (apply-generic 'equ? x y))


;;; a sample compound generic operation
;;;   GN --> GN
(define (square x) (mul x x))


;;; THE NUMBER PACKAGE

;;; Methods for dealing with ordinary numbers.
;;;  RepNum = Sch-Num


;;;   (RepNum, RepNum) --> ({number} X RepNum)
(define (+number x y) (make-number (+ x y)))
(define (-number x y) (make-number (- x y)))
(define (*number x y) (make-number (* x y)))
(define (/number x y) (make-number (/ x y)))

;;;   RepNum --> ({number} X RepNum)
(define (negate-number x) (make-number (- x)))

;;;   RepNum --> Bool
(define (=zero-number? x) (= x 0))

;;;   RepNum --> ({number} X RepNum)
(define (make-number x) (attach-tag 'number x))

;;; (RepNum, RepNum) --> Bool
(define (=number x y) (= x y))


;;; Install the number methods in the generic operations.

(put 'add '(number number) +number)
(put 'sub '(number number) -number)
(put 'mul '(number number) *number)
(put 'div '(number number) /number)

(put 'negate '(number) negate-number)

(put '=zero? '(number) =zero-number?)

(put 'equ? '(number number) =number)


;;; Number Package User Interface

;;; A convenient external  procedure for building a generic
;;; ordinary number from Scheme numbers.

;;; Sch-Num --> ({number} X RepNum)
(define (create-number x) (attach-tag 'number x))

(define n2 (create-number 2))

;;; THE GENERIC RATIONAL PACKAGE

;;; Methods for rationals.

;;;   (RepRat, RepRat) --> ({rational} X RepRat)
(define (+rational x y) (make-rational (+rat x y)))
(define (-rational x y) (make-rational (-rat x y)))
(define (*rational x y) (make-rational (*rat x y)))
(define (/rational x y) (make-rational (/rat x y)))

;;;   RepRat --> ({rational} X RepRat)
(define (negate-rational x) (make-rational (negate-rat x)))

;;;   RepRat --> Bool
(define (=zero-rational? x) (=zero-rat? x))

;;;   RepRat --> ({rational} X RepRat)
(define (make-rational x) (attach-tag 'rational x))

;;;   (RepRat, RepRat) --> Bool
(define (equ-rat? x y) (equ? (mul (denom x) (numer y)) (mul (numer x) (denom y))))

;;;   RepNum --> RepRat
(define (repnum->reprat x) (make-rat (create-number x) (create-number 1)))


;;; Install the rational methods in the generic operations.

(put 'add '(rational rational) +rational)
(put 'sub '(rational rational) -rational)
(put 'mul '(rational rational) *rational)
(put 'div '(rational rational) /rational)

(put 'negate '(rational) negate-rational)
(put '=zero? '(rational) =zero-rational?)
(put 'equ? '(rational rational) equ-rat?)


;;; Rational Package User Interface

;;; A convenient procedure for building a generic rational
;;; from generic numbers.

;;;    (GN, GN) --> ({rational} X RepRat)
(define (create-rational x y)
  (make-rational (make-rat x y)))



;;; THE RATIONAL ARITHMETIC PACKAGE.

;;;   (RepRat, RepRat) --> RepRat

(define (+rat x y)
  (make-rat (add (mul (numer x) (denom y))
                 (mul (denom x) (numer y)))
            (mul (denom x) (denom y))))

(define (-rat x y)
  (make-rat (sub (mul (numer x) (denom y))
                 (mul (denom x) (numer y)))
            (mul (denom x) (denom y))))

(define (*rat x y)
  (make-rat (mul (numer x) (numer y))
            (mul (denom x) (denom y))))

(define (/rat x y)
  (make-rat (mul (numer x) (denom y))
            (mul (denom x) (numer y))))


;;;   RepRat --> RepRat

(define (negate-rat x)
  (make-rat (negate (numer x))
            (denom x)))

;;;   RepRat --> Bool

(define (=zero-rat? x)
  (=zero? (numer x)))


;;; Procedures for representing rationals

;;;   (GN, GN) --> RepRat
(define (make-rat numerator denominator)
  (cons numerator denominator))

;;;   RepRat --> GN
(define numer car)
(define denom cdr)


(define r2 (create-rational (create-number 2) (create-number 1)))
(define r5/13 (create-rational (create-number 5) (create-number 13)))

;;; Coercion procedure from rational/rational method
;;; to number/rational method

;;;  ((RepRat,RepRat) --> T) --> ((RepNum,RepRat) --> T)
(define (RRmethod->NRmethod method)
  (lambda (num rat)
    (method
      (repnum->reprat num)
      rat)))

(put 'add '(number rational) (RRmethod->NRmethod +rational))
(put 'sub '(number rational) (RRmethod->NRmethod -rational))
(put 'mul '(number rational) (RRmethod->NRmethod *rational))
(put 'div '(number rational) (RRmethod->NRmethod /rational))

(put 'equ? '(number rational) (RRmethod->NRmethod equ-rat?))

;;;  ((RepRat,RepRat) --> T) --> ((RepRat,RepNum) --> T)
(define (RRmethod->RNmethod method)
  (lambda (rat num)
    (method
      rat
      (repnum->reprat num))))

(put 'add '(rational number) (RRmethod->RNmethod +rational))
(put 'sub '(rational number) (RRmethod->RNmethod -rational))
(put 'mul '(rational number) (RRmethod->RNmethod *rational))
(put 'div '(rational number) (RRmethod->RNmethod /rational))

(put 'equ? '(rational number) (RRmethod->RNmethod equ-rat?))


;;; THE GENERIC POLYNOMIAL PACKAGE
;;; Methods for polynomials

;;;   (RepPoly, RepPoly) --> ({polynomial} X RepPoly)
(define (+polynomial p1 p2)
  (make-polynomial (+poly p1 p2)))


(define (*polynomial p1 p2)
  (make-polynomial (*poly p1 p2)))

;;;   RepTerms --> RepTerms
(define (negate-terms L)
  (map-terms (lambda (term) (make-term (order term) (negate (coeff term)))) L)) 

;;;   RepPoly --> RepPoly
(define (negate-poly p)
  (make-poly (variable p) (negate-terms (term-list p))))

;;;   RepPoly --> ({polynomial} X RepPoly)
(define (negate-polynomial p)
  (make-polynomial (negate-poly p)))

;;;   (RepPoly, RepPoly) --> RepPoly
(define (-poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (+poly p1
           (negate-poly p2))
    (error "Polys not in same var -- -POLY"
           (list p1 p2))))

;;;   (RepPoly, RepPoly) --> ({polynomial} X RepPoly)
(define (-polynomial p1 p2)
  (make-polynomial (-poly p1 p2)))

;;;   (RepPoly, RepPoly) --> Bool
(define (equ-poly? p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (=zero-poly? (-poly p1 p2))
    #f))

;;;   (RepPoly, RepPoly) --> Bool
(define (equ-polynomial? p1 p2)
  (equ-poly? p1 p2))


;;;   RepPoly --> Bool
(define (=zero-polynomial? p) (=zero-poly? p))


;;;   RepPoly --> ({polynomial} X RepPoly)
(define (make-polynomial poly)
  (attach-tag 'polynomial poly))


;;; Install the polynomial methods in the generic operations.

(put 'add '(polynomial polynomial) +polynomial)
(put 'sub '(polynomial polynomial) -polynomial)
(put 'mul '(polynomial polynomial) *polynomial)

(put '=zero? '(polynomial) =zero-polynomial?)
(put 'equ? '(polynomial polynomial) equ-polynomial?)
(put 'negate '(polynomial) negate-polynomial)

;;; Polynomial Package User Interface

;;; A convenient procedure for building a generic polynomial
;;; from a list of generic numbers, representing the 
;;; coefficients of the polynomial in dense form.

;;;    (Variable, List(GN)) --> ({polynomial} X RepPoly)
(define (create-polynomial var coeffs)
  (make-polynomial (make-poly var (dense/coeffs->sparse/terms coeffs))))

;;;    (Variable, List(Sch-Num)) --> ({polynomial} X RepPoly)
(define (create-numercial-polynomial var coeffs)
  (create-polynomial var (map (lambda (x) (create-number x)) coeffs)))



;;; Makes sparse term representation out of a dense coefficient list
;;;     List(GN) --> Repterms
(define (inc x) (+ x 1))
(define (dense/coeffs->sparse/terms coeffs)
  (define (dt->st rev-coeffs terms degree)
    (if (null? rev-coeffs)
      terms
      (let ((coeff (car rev-coeffs))
            (rev-coeffs (cdr rev-coeffs)))
        (if (=zero? coeff)
          (dt->st rev-coeffs terms (inc degree))
          (dt->st rev-coeffs
                  (adjoin-term (make-term degree coeff)
                               terms)
                  (inc degree))))))
  (dt->st (reverse coeffs) (the-empty-termlist) 0))


;;; THE POLYNOMIAL ARITHMETIC PACKAGE.
;;;    RepPoly = Variable X RepTerms

;;;   (RepPoly, RepPoly) --> RepPoly
(define (+poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (+terms (term-list p1)
                       (term-list p2)))
    (error "Polys not in same var -- +POLY"
           (list p1 p2))))

;;; Need -poly ****

(define (*poly p1 p2)
  (if (same-variable? (variable p1) (variable p2))
    (make-poly (variable p1)
               (*terms (term-list p1)
                       (term-list p2)))
    (error "Polys not in same var -- *POLY"
           (list p1 p2))))

;;;   RepPoly --> RepPoly
;;; Need negate-poly ****


;;;   RepPoly --> Bool
(define (=zero-poly? p)
  (empty-termlist? (term-list p)))

;;;   (Variable, RepTerms) --> RepPoly
(define (make-poly variable term-list)
  (cons variable term-list))

;;;   RepPoly --> Variable
(define (variable p) (car p))

;;;   RepPoly --> RepTerms
(define (term-list p) (cdr p))

;;;   (Variable, Variable) --> Bool
(define (same-variable? v1 v2) (eq? v1 v2))

;;; THE TERM LIST ARITHMETIC PACKAGE.

;;; procedures for dealing with lists of terms in order of 
;;; descending powers.

;;;   (RepTerms, RepTerms) --> RepTerms

(define (+terms L1 L2)
  (cond ((empty-termlist? L1) L2)
        ((empty-termlist? L2) L1)
        (else
          (let ((t1 (first-term L1)) (t2 (first-term L2)))
            (cond ((> (order t1) (order t2))
                   (adjoin-term t1
                                (+terms (rest-terms L1) L2)))
                  ((< (order t1) (order t2))
                   (adjoin-term t2
                                (+terms L1 (rest-terms L2))))
                  (else
                    (adjoin-term (make-term (order t1)
                                            (add (coeff t1)
                                                 (coeff t2)))
                                 (+terms (rest-terms L1)
                                         (rest-terms L2)))))))))

(define (*terms L1 L2)
  (if (empty-termlist? L1)
    (the-empty-termlist)
    (+terms (*-term-by-all-terms (first-term L1) L2)
            (*terms (rest-terms L1) L2))))


;;;   (Proc, RepTerms) --> RepTerms
(define map-terms map)

;;;   (RepTerm, RepTerms) --> RepTerms
(define (*-term-by-all-terms t1 L)
  (map-terms (lambda (term) (*term t1 term)) L))


;;;   (Variable, RepNum) --> RepPoly
(define (repnum->reppoly variable x)
  (make-poly variable (list (make-term 0 (create-number x)))))

;;;  ((RepPoly,RepPoly) --> T) --> ((RepNum,RepPoly) --> T)
(define (PPmethod->NPmethod method)
  (lambda (num poly)
    (method
      (repnum->reppoly (variable poly) num)
      poly)))

(put 'add '(number polynomial) (PPmethod->NPmethod +polynomial))
(put 'sub '(number polynomial) (PPmethod->NPmethod -polynomial))
(put 'mul '(number polynomial) (PPmethod->NPmethod *polynomial))
(put 'equ? '(number polynomial) (PPmethod->NPmethod equ-polynomial?))

;;;  ((RepPoly,RepPoly) --> T) --> ((RepPoly,RepNum) --> T)
(define (PPmethod->PNmethod method)
  (lambda (poly num)
    (method
      poly
      (repnum->reppoly (variable poly) num))))

(put 'add '(polynomial number) (PPmethod->PNmethod +polynomial))
(put 'sub '(polynomial number) (PPmethod->PNmethod -polynomial))
(put 'mul '(polynomial number) (PPmethod->PNmethod *polynomial))
(put 'equ? '(polynomial number) (PPmethod->PNmethod equ-polynomial?))

;;;  (RepTerms, RepNum) --> RepTerms
(define (/terms L num)
  (map-terms (lambda (term) (make-term (order term) (div (coeff term) num))) L))

;;;  (RepPoly, RepNum) --> RepPoly
(define (/poly p num)
  (make-poly (variable p) (/terms (term-list p) num)))

;;;  (RepPoly, RepNum) --> ({polynomial} X RepPoly)
(define (/polynomial p num)
  (make-polynomial (/poly p (create-number num))))

(put 'div '(polynomial number) /polynomial)



;;; Procedures for Representing Term Lists.

;;; RepTerms =  Empty-Term-List  U  (RepTerm X RepTerms)

;;;   constructor of type (RepTerm, RepTerms) --> RepTerms

(define (adjoin-term term term-list)
  (if (=zero? (coeff term))                ;slight simplification
    term-list
    (cons term term-list)))


;;;   constructor of type  EmptyType --> RepTerms
(define (the-empty-termlist) '())

;;; selectors

;;;   RepTerms --> RepTerm
(define (first-term term-list) (car term-list))

;;;   RepTerms --> RepTerms
(define (rest-terms term-list) (cdr term-list))

;;;   RepTerms --> Bool
(define (empty-termlist? term-list) (null? term-list))


;;;  THE TERM ARITHMETIC PACKAGE

;;;   RepTerm = Sch-NatNum X GN

;;;   (Sch-NatNum, GN) --> RepTerm
(define (make-term order coeff) (list order coeff))

;;;   RepTerm --> Sch-NatNum
(define (order term) (car term))

;;;   RepTerm --> GN
(define (coeff term) (cadr term))

;;;   (RepTerm, RepTerm) --> RepTerm
(define (*term t1 t2)
  (make-term
    (+ (order t1) (order t2))
    (mul (coeff t1) (coeff t2))))





(define p1 (create-numercial-polynomial 'x '(1 5 0 -2)))
(define p2
  (create-polynomial
    'z
    (list
      p1
      (create-numercial-polynomial 'x '(3))
      (create-numercial-polynomial 'x '(5)))))
(define p2-mixed
  (create-polynomial
    'z
    (list
      p1
      (create-number 3)
      (create-number 5))))

(define p3
  (create-polynomial
    'x
    (list
      (create-rational
        (create-numercial-polynomial 'y '(3))
        (create-numercial-polynomial 'y '(1 0)))
      (create-rational
        (create-numercial-polynomial 'y '(0))
        (create-numercial-polynomial 'y '(1)))
      (create-rational
        (create-numercial-polynomial 'y '(2 0 1))
        (create-numercial-polynomial 'y '(1 0)))
      (create-rational
        (create-numercial-polynomial 'y '(1))
        (create-numercial-polynomial 'y '(1 -1)))
      (create-rational
        (create-numercial-polynomial 'y '(2))
        (create-numercial-polynomial 'y '(1))))))

;;;APPLYING POLYNOMIALS

(define (apply-term t gn)
  (mul (coeff t)
       (power gn (order t))))

(define (dec x) (- x 1))
(define (power gn k)
  (if (< k 1)
    (create-number 1)
    (mul gn (power gn (dec k)))))

(define (apply-terms terms gn)
  (define (accumulate null-value proc terms)
    (if (null? terms)
      null-value
      (proc (car terms)
            (accumulate null-value proc (cdr terms)))))
  (accumulate (create-number 0) add (map-terms (lambda (term) (apply-term term gn)) terms)))

(define (apply-polynomial p gn)
  (apply-terms
    (term-list (contents p))
    gn))



;;; Test

(define (disp val)
  (display val)
  (newline))

(define (pretty-disp-term var term)
  (pretty-disp (coeff term))
  (if (= (order term) 0)
    'ok
    (begin
      (display " ")
      (display var)
      (display "^")
      (display (order term))
      'ok)))

(define (pretty-disp-polynomial p)
  (let ((terms (term-list p)) (var (variable p)))
    (display "(")
    (if (empty-termlist? terms)
      (display "0")
      (let ((first (first-term terms)))
        (pretty-disp-term var first)
        (map-terms (lambda (term)
               (if (=zero? (coeff term))
                 'ok
                 (begin
                   (display " + ")
                   (pretty-disp-term var term)
                   'ok)))
             (rest-terms terms))))
    (display ")")))

(define (pretty-disp-number n)
  (display n))

(define (pretty-disp-rational r)
  (display "(")
  (pretty-disp (numer r))
  (display "/")
  (pretty-disp (denom r))
  (display ")"))

(put 'pretty-disp '(polynomial) pretty-disp-polynomial)
(put 'pretty-disp '(number) pretty-disp-number)
(put 'pretty-disp '(rational) pretty-disp-rational)

(define (pretty-disp gn)
  (apply-generic 'pretty-disp gn))

(define (show gn)
  (pretty-disp gn)
  (newline))
  

;;; equ? on generic ordinary number
(disp (equ? (create-number 1) (create-number 2)))
(disp (equ? (create-number 2) (create-number 2)))

;;; equ? on generic rational number
(disp (equ? (create-rational (create-number 1) (create-number 2))
            (create-rational (create-number 2) (create-number 4))))
(disp (equ? (create-rational (create-number 0) (create-number 555))
            (create-rational (create-number 0) (create-number 2))))
(disp (equ? (create-rational (create-number 2) (create-number 3))
            (create-rational (create-number 2) (create-number 4))))

;;; operations betwen number and rational
(show (add r2 n2))
(show (mul n2 r2))
(show (sub n2 r5/13))
(show (div r5/13 n2))
(show (div (add r2 (mul n2 n2)) (sub (mul r2 r5/13) n2)))
(disp (equ? n2 r2))
(disp (equ? (sub (add n2 r5/13) r5/13) n2))

;;; square of polynomial
(show (square p1))
(show (square (square p1)))
(show (square p2))
(show (square (square p2)))
(show (square p3))

;;; negate, sub, equ? on p1, p2, p3
(show (sub p1 p3))
(disp (equ? p1 p1))
(disp (equ? p2 p3))
(disp (equ? p3 p1))
(show (negate p2))
(show (negate p3))

;;; operations between number and polynomial
(show (square p2-mixed))
(disp (equ? (sub (add p1 p3) p1) p3))
(show (div p1 n2))

;;; apply-polynomial
(show (apply-polynomial p1 (create-number 2)))
(show (apply-polynomial p2 (create-numercial-polynomial 'x '(1 1))))
(define x (create-numercial-polynomial 'x '(1 0)))
(disp (equ? (apply-polynomial p1 x) p1))
