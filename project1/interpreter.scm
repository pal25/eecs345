(load "verySimpleParser.scm")
(load "environment.scm")
(load "exprs.scm")

(define interpret
  (lambda (filename)
    (env-lookup 'return (interpret-stmt-list
		     (parser filename)
		     newenv))))

(define interpret-stmt-list
  (lambda (parsetree env)
    (cond
     ((null? parsetree) env)
     (else (interpret-stmt-list (cdr parsetree)
				(interpret-stmt (car parsetree)
						env))))))

(define interpret-stmt
  (lambda (stmt env)
    (cond
     ((eq? '= (car stmt))
      (interpret-assign stmt env))
     ((eq? 'var (car stmt))
      (interpret-var stmt env))
     ((eq? 'if (car stmt))
      (interpret-if stmt env))
     ((eq? 'return (car stmt))
      (interpret-return stmt env)))))

(define interpret-assign
  (lambda (stmt env)
    (cond
     ;save room for return value
     (else
      (env-update (LHS stmt) (RHS stmt) env)))))

(define LHS
  (lambda (stmt)
    (car (cdr stmt))))

(define RHS
  (lambda (stmt)
    (car (cdr (cdr stmt)))))

(define interpret-var
  (lambda (stmt env)
    (cond
     ((null? (cdr (cdr stmt)))
      (env-bind (LHS stmt) 'NEWVAR env))
     ((expression? (RHS stmt))
      (env-bind (LHS stmt) (value (RHS stmt) env) env)))))

(define interpret-return
  (lambda (stmt env)
    (cond
    ((expression? (LHS stmt))
     (env-bind 'return (value (LHS stmt) env) env))
    (else
     (env-bind 'return (env-lookup (LHS stmt) env))))))

(define interpret-if
  (lambda (stmt env)
    (cond
     ((value (if-conditional stmt) env)
      (interpret-stmt (if-thenstmt stmt) env))
     ((not (null? (if-optelse stmt)))
      (interpret-if (if-optelse stmt) env)))))
     
(define if-conditional
  (lambda (stmt)
    (car (cdr stmt))))

(define if-thenstmt
  (lambda (stmt)
    (car (cdr (cdr stmt)))))

(define if-optelse
  (lambda (stmt)
    (cond
     ((null? (cdr (cdr (cdr stmt)))) '())
     (else (car (cdr (cdr (cdr stmt))))))))