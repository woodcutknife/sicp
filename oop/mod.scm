;;;;mod:

;;;; The entire evaluator code for the problem set is contained in the file teval.scm.
;;;; The two procedures here are the only ones that you should need to
;;;; modify for the problem set (except for the extra credit part).


(define (eval-define-method exp env)
  (let ((var (method-definition-generic-function exp)))
    (if (variable? var)
      (let ((b (binding-in-env var env)))
        (if (or 
              (not (found-binding? b))
              (not (generic-function? (binding-value b))))
          (let ((val (make-generic-function var)))
            (define-variable! var val env))))))
  (let ((gf (tool-eval (method-definition-generic-function exp) env)))
    (if (not (generic-function? gf))
        (error "Unrecognized generic function -- DEFINE-METHOD >> "
               (method-definition-generic-function exp))
        (let ((params (method-definition-parameters exp)))
          (install-method-in-generic-function
           gf
           (map (lambda (p) (paramlist-element-class p env))
                params)
           (make-procedure (make-lambda-expression
                            (map paramlist-element-name params)
                            (method-definition-body exp))
                           env))
          (list 'added 'method 'to 'generic 'function:
                (generic-function-name gf))))))


(define (eval-define-class exp env)
  (let ((superclass (tool-eval (class-definition-superclass exp)
                               env)))
    (if (not (class? superclass))
        (error "Unrecognized superclass -- MAKE-CLASS >> "
               (class-definition-superclass exp))
        (let ((name (class-definition-name exp))
              (all-slots (collect-slots
                          (class-definition-slot-names exp)
                          superclass)))
          (let ((new-class
                 (make-class name superclass all-slots)))
            (define-variable! name new-class env)
            (for-each
              (lambda (slot-name)
                (tool-eval `(define-method ,slot-name ((obj ,name)) (get-slot obj ',slot-name))
                           env))
              all-slots)
            (list 'defined 'class: name))))))
