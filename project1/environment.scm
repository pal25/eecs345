(define newenv '())

(define lookup
  (lambda (var env)
    (cond
     ((null? env) (error "var not found"))
     ((eq? var (car (car env))) (cdr (car env)))
     (else (lookup var (cdr env))))))

(define bind
  (lambda (var val env)
    (cons (cons var (cons val '())) env)))