(load "loopSimpleParser.scm")
(load "environment.scm")

(define interpret
  (lambda (filename)
    (env-lookup 'return (interpret-stmt-list (parser filename) newenv))))

(define interpret-stmt-list
  (lambda (parsetree env)
    (cond
     ((null? parsetree) env)
     (else (interpret-stmt-list (cdr parsetree) (interpret-stmt (car parsetree)	env))))))

(define interpret-stmt
  (lambda (stmt env)
    (cond
     ((eq? '= (car stmt)) (interpret-assign stmt env))
     ((eq? 'var (car stmt)) (interpret-var stmt env))
     ((eq? 'if (car stmt)) (interpret-if stmt env))
     ((eq? 'return (car stmt)) (interpret-return stmt env))
     (else (error "Error: Not a valid statement")))))

(define interpret-assign
  (lambda (stmt env)
    (env-update (LHS stmt) (interpret-value (RHS stmt) env) env)))
  
(define interpret-var
  (lambda (stmt env)
    (cond
     ((env-declared? (LHS stmt) env) (error "Error: Cant redeclare variables"))
     ((null? (cddr stmt)) (env-bind (LHS stmt) '() env))
     (else (env-bind (LHS stmt) (interpret-value (RHS stmt) env) env)))))
  
(define interpret-return
  (lambda (stmt env)
    (env-bind 'return (interpret-value (LHS stmt) env) env)))
  
(define interpret-if
  (lambda (stmt env)
    (cond
     ((eq? (interpret-value (cadr stmt) env) 'true) (interpret-stmt (caddr stmt) env))
     ((interpret-else? stmt) (interpret-stmt (cadddr stmt) env))
     (else env))))
     
(define interpret-else?
  (lambda (stmt)
    (cond
     ((null? (cdddr stmt)) #f)
     (else #t))))

(define interpret-value
  (lambda (stmt env)
    (cond
     ((null? stmt) '())
     ((number? stmt) stmt)
     ((eq? stmt 'true) 'true)
     ((eq? stmt 'false) 'false)
     ((atom? stmt) (env-lookup stmt env))
     ((eq? '+ (operator stmt)) ((interpret-binary +) stmt env))
     ((eq? '- (operator stmt)) ((interpret-negative -) stmt env))
     ((eq? '* (operator stmt)) ((interpret-binary *) stmt env))
     ((eq? '/ (operator stmt)) ((interpret-binary quotient) stmt env))
     ((eq? '% (operator stmt)) ((interpret-binary remainder) stmt env))
     ((eq? '> (operator stmt)) ((interpret-boolean >) stmt env))
     ((eq? '< (operator stmt)) ((interpret-boolean <) stmt env))
     ((eq? '>= (operator stmt)) ((interpret-boolean >=) stmt env))
     ((eq? '<= (operator stmt)) ((interpret-boolean <=) stmt env))
     ((eq? '!= (operator stmt)) ((interpret-boolean (lambda (a b) (not (eq? a b)))) stmt env))
     ((eq? '== (operator stmt)) ((interpret-boolean (lambda (a b) (eq? a b))) stmt env))
     ((eq? '|| (operator stmt)) ((interpret-boolean (lambda (a b)
						      (cond
						       ((and (eq? a 'true) (eq? b 'true)) #t)
						       ((and (eq? a 'true) (eq? b 'false)) #t)
						       ((and (eq? a 'false) (eq? b 'true)) #t)
						       ((and (eq? a 'false) (eq? b 'false)) #f))))
				 stmt env))
     ((eq? '&& (operator stmt)) ((interpret-boolean (lambda (a b)
						       (cond
						       ((and (eq? a 'true) (eq? b 'true)) #t)
						       ((and (eq? a 'true) (eq? b 'false)) #f)
						       ((and (eq? a 'false) (eq? b 'true)) #f)
						       ((and (eq? a 'false) (eq? b 'false)) #f))))
				 stmt env))
     ((eq? '! (operator stmt)) ((interpret-unary-boolean (lambda (a) (cond ((eq? a 'true) #f)
									   ((eq? a 'false) #t))))
				stmt env))
     (else (error "Invalid expression")))))

(define interpret-unary-boolean
  (lambda (op)
    (lambda (stmt env)
      (cond
       ((op (interpret-value (operand1 stmt) env)) 'true)
       (else 'false)))))

(define interpret-boolean
  (lambda (op)
    (lambda (stmt env)
      (cond
       ((op (interpret-value (operand1 stmt) env) (interpret-value (operand2 stmt) env)) 'true)
       (else 'false)))))
	    

(define interpret-binary
  (lambda (op)
    (lambda (stmt env)
      (op (interpret-value (operand1 stmt) env)
	  (interpret-value (operand2 stmt) env)))))

(define interpret-negative
  (lambda (op)
    (lambda (stmt env)
      (cond
       ((null? (cddr stmt)) (* -1 (interpret-value (operand1 stmt) env)))
       (else (op (interpret-value (operand1 stmt) env)
		 (interpret-value (operand2 stmt) env)))))))

(define operator
  (lambda (expr)
    (car expr)))

(define operand1
  (lambda (expr)
    (car (cdr expr))))

(define operand2
  (lambda (expr)
    (car (cdr (cdr expr)))))

(define LHS
  (lambda (stmt)
    (car (cdr stmt))))

(define RHS
  (lambda (stmt)
    (car (cdr (cdr stmt)))))

(define atom?
  (lambda (stmt)
    (not (or (pair? stmt) (null? stmt)))))