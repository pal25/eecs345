(define newenv '((() ())))

(define env-push-layer
  (lambda (env)
    (cons '(() ()) env)))

(define env-pop-layer
  (lambda (env)
    (cdr env)))

(define in-layer?
  (lambda (var layer)
    (cond
     ((null? (top-var layer)) #f)
     ((eq? var (top-var layer)) #t)
     (else (in-layer? var (layer-pop-top layer))))))

(define env-lookup
  (lambda (var env)
    (cond
     ((null? env) (error "Error: Variable not in environment"))
     ((in-layer? var (top-layer env)) (env-lookup-layer var (top-layer env)))
     (else (env-lookup var (env-pop-layer env))))))

(define env-lookup-layer
  (lambda (var layer)
    (cond
     ((null? (top-var layer)) (top-var layer))
     ((eq? (top-var layer) var) (top-val layer))
     (else (env-lookup-layer var (layer-pop-top layer))))))

(define env-bind
  (lambda (var val env)
    (cons (cons (cons var (car (top-layer env))) 
		(cons (cons val (cadr (top-layer env))) '()))
	  (env-pop-layer env))))

(define env-update
  (lambda (var val env)
    (letrec ((env-update-checked 
	      (lambda (var val env)
		(cond
		 ((in-layer? var (top-layer env)) (env-bind var val env))
		 (else (cons (top-layer env) (env-update-checked var val (env-pop-layer env))))))))
      (cond
       ((not (env-declared? var env)) (error "Error: Variable not declared"))
       (else (env-update-checked var val env))))))

(define env-declared?
  (lambda (var env)
    (cond
     ((null? env) #f)
     ((in-layer? var (top-layer env)) #t)
     (else (env-declared? var (env-pop-layer env))))))

(define top-var
  (lambda (layer)
    (cond
     ((null? (car layer)) '())
     (else (car (car layer))))))

(define top-val
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