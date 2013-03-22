(define newenv '(()))

(define env-push-layer
  (lambda (env)
    (cons '() env)))

(define env-pop-layer
  (lambda (env)
    (cdr env)))

(define env-lookup
  (lambda (var env)
    (cond
     ((null? env) (error "Error: Variable not in environment"))
     ((env-declared-layer? var (top-layer env)) (env-lookup-layer var (top-layer env)))
     (else (env-lookup var (cdr env))))))

(define env-lookup-layer
  (lambda (var layer)
    (cond
     ((null? layer) '())
     ((eq? var (top-var layer)) (top-val layer))
     (else (env-lookup-layer var (cdr layer))))))

(define env-bind
  (lambda (var val env)
    (cons (cons (cons var (cons (interpret-value val env) '())) (top-layer env)) (cdr env))))

(define env-bind-layer
  (lambda (var val layer)
    (cond
     ((eq? var (car (car layer))) (cons (cons var (cons (interpret-value val layer) '())) (cdr layer)))
     (else (cons (car layer) (env-bind-layer var val (cdr layer)))))))

(define env-update
  (lambda (var val env)
    (cond
     ((null? env) (error "Error: Variable undeclared"))
     ((env-declared-layer? var (top-layer env)) (cons (env-bind-layer var val (top-layer env)) (cdr env)))
     (else (cons (top-layer env) (env-update var val (cdr env)))))))

(define env-declared?
  (lambda (var env)
    (cond
     ((null? env) #f)
     ((env-declared-layer? var (top-layer env)) #t)
     (else (env-declared? var (cdr env))))))

(define env-declared-layer?
  (lambda (var env)
    (cond
     ((null? env) #f)
     ((eq? var (top-var env)) #t)
     (else (env-declared-layer? var (cdr env))))))

(define top-var
  (lambda (layer)
    (car (car layer))))

(define top-val
  (lambda (layer)
    (cadr (car layer))))

(define top-layer
  (lambda (env)
    (car env)))