(define newenv '())

(define env-lookup
  (lambda (var env)
    (cond
     ((not (env-declared? var env)) (error "Error: Variable not in environment"))
     ((eq? var (first-var env)) (first-val env))
     (else (env-lookup var (cdr env))))))

(define env-bind
  (lambda (var val env)
    (cond
     ((eq? newenv val) (cons (cons var (cons val newenv)) env))
     (else (cons (cons var (cons (interpret-value val env) '())) env)))))

(define env-update
  (lambda (var val env)
    (cond
     ((eq? (env-lookup var env) 'newvar) (error "Error: Variable undeclared"))
     (else (env-bind var val env)))))

(define env-declared?
  (lambda (var env)
    (cond
     ((null? env) #f)
     ((eq? var (first-var env)) #t)
     (else (env-declared? var (cdr env))))))

(define first-var
  (lambda (env)
    (car (car env))))

(define first-val
  (lambda (env)
    (car (cdr (car env)))))