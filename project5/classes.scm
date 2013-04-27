(load "environment.scm")

(define interpret-class-declare
  (lambda (class env)
    (let* ((name (cadr class))
	   (body (cadddr class))
	   (parent (if (null? (caddr class)) '() (cadr (caddr class))))
	   (class-env (list empty-env empty-env empty-env parent)))
      (env-bind name (interpret-class-list body class-env) env))))

(define interpret-class-list
  (lambda (body class-env)
    (cond
     ((null? body) class-env)
     (else (interpret-class-list (cdr body) (interpret-class-stmt (car body) class-env))))))

(define interpret-class-stmt
  (lambda (stmt class-env)
    (cond
     ((eq? 'static-function (car stmt)) (list 
					 (class-varenv class-env)
					 (inst-varenv class-env)
					 (interpret-func-declare stmt (class-methodenv class-env))
					 (class-parent class-env)))
     ((eq? 'static-var (car stmt)) (list
				    (interpret-var stmt (class-varenv class-env))
				    (inst-varenv class-env)
				    (class-methodenv class-env)
				    (class-parent class-env))))))


; class-newenv = (class-varenv inst-varenv methodenv parent)
(define class-newenv (list empty-env empty-env empty-env 'None))

(define class-varenv (lambda (class-env) (car class-env)))
(define inst-varenv (lambda (class-env) (cadr class-env)))
(define class-methodenv (lambda (class-env) (caddr class-env)))
(define class-parent (lambda (class-env) (cadddr class-env)))

(define class-method-lookup 
  (lambda (method class-env) 
    (env-lookup-layer method (class-methodenv class-env) top-val-box)))

(define class-var-lookup 
  (lambda (var class-env) 
    (env-lookup-layer var (class-varenv class-env) top-val-box)))

; inst-newenv = (inst-varvals type)
(define inst-newenv '(() None))