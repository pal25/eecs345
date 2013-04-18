(define interpret-func-declare
  (lambda (stmt env)
    (env-bind (cadr stmt) (cddr stmt) env)))

(define interpret-func-call
  (lambda (func-name values env class-env instance-env)
    (cond
     ((eq? 'void (env-lookup 'return (return-env func-name values env))) env)
     (else (env-lookup 'return (return-env func-name values env))))))

(define return-env
  (lambda (func-name values env)
    (call/cc
     (lambda (return)
       (interpret-stmt-list (cadr (env-lookup func-name env))
			    (create-func-env (car (env-lookup func-name env)) values env)
			    return undef-break undef-continue)))))

(define interpret-called-values
  (lambda (values env)
    (cond
     ((null? values) '())
     (else (cons (interpret-value (car values) env) (interpret-called-values (cdr values) env))))))