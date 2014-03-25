;;;; Main

; 符号求导
(define (deriv exp var)
  (install-packages)  
  (cond ((number? exp) 0)
        ((variable? exp)
         (if (same-variable? exp var) 1 0))
        (else (simplify ((get 'deriv (operator exp)) (operands exp) var)))))

; 符号化简
(define (simplify exp)
  (install-packages)
  (if (or (number? exp) (variable? exp))
    exp
    ((get 'simplify (operator exp)) (operands exp))))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2) (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

;;;; Basic construct functions

; 负式
(define (make-nega a) (list '- a))
; 和式
(define (make-sum a1 a2) (list '+ a1 a2))
; 积式
(define (make-product m1 m2) (list '* m1 m2))
; 商式
(define (make-division m1 m2) (list '/ m1 m2))
; 乘方
(define (make-expontiation b e) (list '** b e))
; 函数
(define (make-func func arg) (list func arg))

;;;; Tools

(define (=number? exp num)
  (and (number? exp) (= exp num)))

; 获得表达式的类型
(define (type-tag exp)
  (cond ((number? exp) 'number)
        ((variable? exp) 'variable)
        (else (car exp))))

; 将opeands里表达式类型为op的项展开，即 (flatten-if-exp-is '+ '(1 (+ 2 3))) 会得到 '(1 2 3)
(define (flatten-if-exp-is op operands)
  (accumulate append 
              '()
              (map
                (lambda (item)
                  (if (pair? item)
                    (if (eq? (car item) op)
                      (cdr item)
                      (list item))
                    (list item)))
                operands)))

(define (accumulate op init seq)
  (if (null? seq)
    init
    (op (car seq)
        (accumulate op init (cdr seq)))))

;;;; Packages

; 安装，将对不同类型的表达式的求导和化简过程进行加载
(define packages-installed? #f)
(define (install-packages)
  (if packages-installed?
    #t
    (begin 
      (install-nega-package)
      (install-sum-package)
      (install-product-package)
      (install-division-package)
      (install-expontiation-package)
      (install-functions-package)
      (set! packages-installed? #t)
      #t)))

; 负式
(define (install-nega-package)
  (define (arg operands) (car operands))

  (define (deriv-proc operands var)
    (make-nega (deriv (arg operands) var)))
  
  (define (simplify-proc operands)
    (let ((res (simplify (arg operands))))
      (cond ((number? res) (- res))
            (else (make-nega res)))))

  (put 'deriv '- deriv-proc)
  (put 'simplify '- simplify-proc))

; 和式
(define (install-sum-package)
  (define (addend s) (car s))
  (define (augend s)
    (if (< (length s) 3)
      (cadr s)
      (cons '+ (cdr s))))

  (define (deriv-proc operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))

  (define (simplify-proc operands)
    (let ((simplified-operands (flatten-if-exp-is '+ (map simplify operands))))
      (let ((numbers-sum (accumulate + 0 (filter (lambda (x) (eq? (type-tag x) 'number)) simplified-operands)))
            (rest-items (filter (lambda (x) (not (eq? (type-tag x) 'number))) simplified-operands)))
        (if (= numbers-sum 0)
          (cond ((null? rest-items) 0)
                ((< (length rest-items) 2) (car rest-items))
                (else (cons '+ rest-items)))
          (cond ((null? rest-items) numbers-sum)
                (else (append (list '+ numbers-sum) rest-items)))))))

  (put 'deriv '+ deriv-proc)
  (put 'simplify '+ simplify-proc))

; 积式
(define (install-product-package)
  (define (multiplier p) (car p))
  (define (multiplicand p)
    (if (< (length p) 3)
      (cadr p)
      (cons '* (cdr p))))

  (define (deriv-proc operands var)
    (make-sum
      (make-product (deriv (multiplier operands) var)
                    (multiplicand operands))
      (make-product (multiplier operands)
                    (deriv (multiplicand operands) var))))

  (define (simplify-proc operands)
    (let ((simplified-operands (flatten-if-exp-is '* (map simplify operands))))
      (let ((numbers-product (accumulate * 1 (filter (lambda (x) (eq? (type-tag x) 'number)) simplified-operands)))
            (rest-items (filter (lambda (x) (not (eq? (type-tag x) 'number))) simplified-operands)))
        (cond ((= numbers-product 0) 0)
              ((= numbers-product 1)
               (cond ((null? rest-items) 1)
                     ((< (length rest-items) 2) (car rest-items))
                     (else (cons '* rest-items))))
              ((= numbers-product -1)
               (cond ((null? rest-items) -1)
                     ((< (length rest-items) 2) (make-nega (car rest-items)))
                     (else (make-nega (cons '* rest-items)))))
              (else
                (cond ((null? rest-items) numbers-product)
                      (else (append (list '* numbers-product) rest-items))))))))

  (put 'deriv '* deriv-proc)
  (put 'simplify '* simplify-proc))

; 商式
(define (install-division-package)
  (define (dividend e) (car e))
  (define (divisor e) (cadr e))

  (define (deriv-proc operands var)
    (make-division
      (make-sum
        (make-product
          (deriv (dividend operands) var)
          (divisor operands))
        (make-nega
          (make-product
            (dividend operands)
            (deriv (divisor operands) var))))
      (make-expontiation (divisor operands) 2)))

  (define (simplify-proc operands)
    (let ((m1 (simplify (dividend operands)))
          (m2 (simplify (divisor operands))))
      (cond ((=number? m1 0) 0)
            ((and (number? m1) (number? m2)) (/ m1 m2))
            ((and (variable? m1) (variable? m2) (eq? m1 m2)) 1)
            ((=number? m2 1) m1)
            ((=number? m2 -1) (make-nega m1))
            (else (make-division m1 m2)))))

  (put 'deriv '/ deriv-proc)
  (put 'simplify '/ simplify-proc))

; 乘方
(define (install-expontiation-package)
  (define (base e) (car e))
  (define (exponent e) (cadr e))

  (define (deriv-proc operands var)
    (make-product
      (make-expontiation (base operands) (exponent operands))
      (make-sum
        (make-product
          (deriv (exponent operands) var)
          (make-func 'log (base operands)))
        (make-product
          (make-division (exponent operands) (base operands))
          (deriv (base operands) var)))))

  (define (simplify-proc operands)
    (let ((b (simplify (base operands)))
          (e (simplify (exponent operands))))
      (cond ((and (or (integer? b) (ratnum? b)) (integer? e)) (expt b e))
            ((=number? e 0) 1)
            ((=number? e 1) b)
            (else (make-expontiation b e)))))

  (put 'deriv '** deriv-proc)
  (put 'simplify '** simplify-proc))

; 函数
(define (install-functions-package)
  (define (arg operands) (car operands))

  ; 对函数的求导和化简进行安装
  (define (install-func func proc)
    (put 'deriv
         func
         (lambda (operands var)
           (make-product
             (proc (arg operands))
             (deriv (arg operands) var))))
    (put 'simplify
         func
         (lambda (operands)
           (define res (simplify (arg operands)))
           (make-func func res))))

  (install-func 'log
                (lambda (x)
                  (make-expontiation x -1)))
  (install-func 'exp
                (lambda (x)
                  (make-func 'exp x)))
  (install-func 'sin
                (lambda (x)
                  (make-func 'cos x)))
  (install-func 'cos
                (lambda (x)
                  (make-nega (make-func 'sin x))))
  (install-func 'tan
                (lambda (x)
                  (make-expontiation (make-expontiation (make-func 'cos x)
                                                        2)
                                     -1)))
  (install-func 'arcsin
                (lambda (x)
                  (make-expontiation (make-sum 1
                                               (make-nega (make-expontiation x 2)))
                                     -1/2)))
  (install-func 'arccos
                (lambda (x)
                  (make-nega (make-expontiation (make-sum 1
                                                          (make-nega (make-expontiation x 2)))
                                                -1/2))))
  (install-func 'arctan
                (lambda (x)
                  (make-expontiation (make-sum 1
                                               (make-expontiation x 2))
                                     -1))))

;;;; Operation table

; 定义了一个二维表格，对不同的表达式类型记录了相应的求导和化简过程

(define (assoc key records)
  (cond ((null? records) #f)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (make-table)
  (let ((local-table (list '*table*)))
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
                    (cons (list key-1 (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              #f))
          #f)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
