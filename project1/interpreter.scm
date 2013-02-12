(load "verySimpleParser.scm")
(load "environment.scm")
(load "exprs.scm")

(define interpret
  (lambda (filename)
    (lookup 'return (interpret-statement-list
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
      (interpret-return stmt env))

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
      (env-bind (LHS stmt) '() env))
     ((expression? (RHS stmt))
      (env-bind (LHS stmt) (value (RHS stmt) env) env)))))