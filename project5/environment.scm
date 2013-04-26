(define newenv (cons (cons 
		            (cons (box 'return) '())
			          (cons (cons (box 'void) '())
					    '()))
		          '()))

(define create-func-env
  (lambda (formal-params values env)
    (letrec ((add-bindings 
	            (lambda (formal values env newenv)
		      (cond
		        ((null? formal) newenv)
			 ((eq? (car formal) '&) (add-bindings (cddr formal) (cdr values) env (env-bind-box (cadr formal) (env-lookup-extra (car values) env top-val-box) newenv)))
			  (else (add-bindings (cdr formal) (cdr values) env (env-bind (car formal) (interpret-value (car values) env) newenv)))))))
      (add-bindings formal-params values env (env-push-layer (env-global-layer env))))))

(define env-global-layer
  (lambda (env)
    (cond
     ((null? (cdr env)) env)
     (env-global-layer (cdr env)))))

(define env-push-layer
  (lambda (env)
    (cons '(() ()) env)))

(define env-pop-layer
  (lambda (env)
    (cdr env)))

(define env-declared-layer?
  (lambda (var layer)
    (cond
     ((null? (top-var layer)) #f)
     ((eq? var (top-var layer)) #t)
     (else (env-declared-layer? var (layer-pop-top layer))))))

(define env-lookup
  (lambda (var env)
    (env-lookup-extra var env top-val)))

(define env-lookup-extra
  (lambda (var env get-top-val)
    (cond
     ((null? env) (error "Error: Variable not in environment"))
     ((env-declared-layer? var (top-layer env)) (env-lookup-layer var (top-layer env) get-top-val))
     (else (env-lookup-extra var (env-pop-layer env) get-top-val)))))

(define env-lookup-layer
  (lambda (var layer get-top-val)
    (cond
     ((null? (top-var layer)) (top-var layer))
     ((eq? (top-var layer) var) (get-top-val layer))
     (else (env-lookup-layer var (layer-pop-top layer) get-top-val)))))

(define env-bind
  (lambda (var val env)
    (cons (cons (cons (box var) (car (top-layer env))) 
		(cons (cons (box val) (cadr (top-layer env))) '()))
	    (env-pop-layer env))))

(define env-bind-box
  (lambda (var val env)
    (cons (cons (cons (box var) (car (top-layer env)))
		(cons (cons val (cadr (top-layer env))) '()))
	    (env-pop-layer env))))

(define env-update
  (lambda (var val env)
    (letrec ((env-update-checked 
	            (lambda (var val env)
		      (cond
		        ((env-declared-layer? var (top-layer env)) (begin (set-box! (env-lookup-extra var env top-val-box) val) env))
			 (else (cons (top-layer env) (env-update-checked var val (env-pop-layer env))))))))
      (cond
       ((not (env-declared? var env)) (error "Error: Variable not declared"))
       (else (env-update-checked var val env))))))

(define env-declared?
  (lambda (var env)
    (cond
     ((null? env) #f)
     ((env-declared-layer? var (top-layer env)) #t)
     (else (env-declared? var (env-pop-layer env))))))

(define top-var
  (lambda (layer)
    (cond
     ((null? (car layer)) '())
     (else (unbox (car (car layer)))))))

(define top-val
  (lambda (layer)
    (cond
     ((null? (cadr layer)) '())
     (else (unbox (caadr layer))))))

(define top-val-box
  (lambda (layer)
    (cond
     ((null? (cadr layer)) '())
     (else (caadr layer)))))

(define top-layer
  (lambda (env)
    (car env)))

(define layer-pop-top
  (lambda (layer)
    (cons (cdar layer) (cons (cdadr layer) '()))))
