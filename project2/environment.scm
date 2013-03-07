(define newenv '())

(define env-lookup
  (lambda (var env)
    (cond
     ((null? env) '())
     ((eq? var (first-var env)) (first-val env))
     (else (env-lookup var (cdr env))))))

(define env-bind
  (lambda (var val env)
    (cond
     ((expression? val)
      (cons (cons var (cons (value val env) '())) env))
     (else
      (cons (cons var (cons val '())) env)))))

(define env-update
  (lambda (var val env)
    (cond
     ((null? (env-lookup var env)) ; Only to check 
      (error "Variable undeclared"))
     (else
      (env-bind var val env)))))

(define first-var
  (lambda (env)
    (car (car env))))

(define first-val
  (lambda (env)
    (car (cdr (car env)))))