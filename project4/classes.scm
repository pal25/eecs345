(define interpret-class-declare
  (lambda (name body extends parent inst-vars env)
    (env-bind name (interpret-class-list body (env-update 'parent parent (env-update 'extends extends new-class-env))) env)))
	     
(define interpret-class-list
  (lambda (parsetree env)
    (cond
     ((null? parsetree) env)
     (else (interpret-class-list (cdr parsetree) (interpret-class-stmt (car parsetree) env))))))

(define interpret-class-stmt
  (lambda (stmt class-env)
    (cond
     ((eq? 'static-function (car stmt)) (env-bind 'method-env (interpret-func-declare stmt (class-method-env class-env)) class-env))
     ((eq? 'static-var (car stmt)) (env-bind 'var-env (interpret-var stmt (class-var-env class-env)) class-env)))))

;class env = (class-var-env instance-vars-env class-method-env extends parent-class)
(define new-class-env (env-bind 'var-env newenv
				(env-bind 'inst-env newenv
					  (env-bind 'method-env newenv
						    (env-bind 'extends 'None
							      (env-bind 'parent 'None newenv))))))

(define class-var-env
  (lambda (class-env)
    (env-lookup 'var-env class-env)))

(define class-inst-env
  (lambda (class-env)
    (env-lookup 'inst-env class-env)))

(define class-method-env
  (lambda (class-env)
    (env-lookup 'method-env class-env)))

(define class-parent
  (lambda (class-env)
    (env-lookup 'parent class-env)))

(define class-extends
  (lambda (class-env)
    (env-lookup 'extends class-env)))


    
    