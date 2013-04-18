(define interpret-func-declare
  (lambda (stmt env)
    (env-bind (cadr stmt) (cddr stmt) env)))

(define interpret-func-call
  (lambda (func-name values env cls inst)
    (cond
     ((eq? 'void (env-lookup 'return (return-env func-name values env cls inst))) env)
     (else (env-lookup 'return (return-env func-name values env cls inst))))))

(define return-env
  (lambda (func-name values env cls inst)
    (call/cc
     (lambda (return)
       (let ((methods (env-lookup 'method-env (env-lookup cls env))))
	 (interpret-stmt-list (cadr (env-lookup func-name methods))
			      (create-func-env (car (env-lookup func-name methods)) values env)
			      cls inst
			      return undef-break undef-continue))))))

(define interpret-called-values
  (lambda (values env)
    (cond
     ((null? values) '())
     (else (cons (interpret-value (car values) env) (interpret-called-values (cdr values) env))))))
