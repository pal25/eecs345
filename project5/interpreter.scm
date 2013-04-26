(load "functionParser.scm")
(load "environment.scm")

(define call/cc call-with-current-continuation)

(define interpret
  (lambda (filename)
    (interpret-func-call 'main '() (interpret-global-stmt-list (parser filename) newenv))))

(define interpret-global-stmt-list
  (lambda (parsetree env)
    (cond
     ((null? parsetree) env)
     (else (interpret-global-stmt-list (cdr parsetree) (interpret-global-stmt (car parsetree) env))))))

(define interpret-global-stmt
  (lambda (stmt env)
    (cond
     ((eq? 'function (car stmt)) (interpret-func-declare stmt env))
     (else (interpret-stmt stmt env undef-return undef-break undef-continue)))))

(define interpret-func-declare
  (lambda (stmt env)
    (env-bind (cadr stmt) (cddr stmt) env)))

(define interpret-stmt-list
  (lambda (parsetree env return break continue)
    (cond
     ((null? parsetree) env)
     (else (interpret-stmt-list (cdr parsetree) 
				(interpret-stmt (car parsetree)	env return break continue)
				return break continue)))))

(define interpret-func-call
  (lambda (func-name values env)
    (cond
     ((eq? 'void (env-lookup 'return (return-env func-name values env))) (env-pop-layer (return-env func-name values env)))
     (else (env-lookup 'return (return-env func-name values env))))))

(define return-env
  (lambda (func-name values env)
    (call/cc
     (lambda (return)
       (interpret-stmt-list (cadr (env-lookup func-name env))
			    (create-func-env (car (env-lookup func-name env)) (interpret-called-values values env) env)
			    return undef-break undef-continue)))))

(define interpret-called-values
  (lambda (values env)
    (cond
     ((null? values) '())
     (else (cons (interpret-value (car values) env) (interpret-called-values (cdr values) env))))))

(define interpret-stmt
  (lambda (stmt env return break continue)
    (cond
     ((eq? '= (car stmt)) (interpret-assign stmt env))
     ((eq? 'var (car stmt)) (interpret-var stmt env))
     ((eq? 'if (car stmt)) (interpret-if stmt env return break continue))
     ((eq? 'while (car stmt)) (interpret-while stmt env return))
     ((eq? 'break (car stmt)) (break env))
     ((eq? 'continue (car stmt)) (continue env))
     ((eq? 'begin (car stmt)) (interpret-begin stmt env return break continue))
     ((eq? 'return (car stmt)) (return (interpret-return stmt env)))
     ((eq? 'funcall (car stmt)) (interpret-func-call (cadr stmt) (cddr stmt) env))
     (else (error "Error: Not a valid statement")))))

(define interpret-sidefx
  (lambda (stmt env)
    (cond
     ((number? stmt) env)
     ((atom? stmt) env)
     ((eq? '! (car stmt)) (interpret-sidefx (LHS stmt) env))
     ((and (eq? '- (car stmt)) (eq? 2 (length stmt))) (interpret-sidefx (LHS stmt) env))
     ((member? (car stmt) '(+ - * / % == != > >= < <= && ||)) 
      (interpret-sidefx (RHS stmt) (interpret-sidefx (LHS stmt) env)))
     ((eq? 'begin (car stmt)) env)
     ((eq? 'break (car stmt)) env)
     ((eq? 'funcall (car stmt)) env)
     (else (interpret-stmt stmt env undef-return undef-break undef-continue)))))

(define interpret-begin
  (lambda (stmt env return break continue)
    (let ((pop-break (lambda (break-env) (break (env-pop-layer break-env))))
	  (pop-continue (lambda (continue-env) (continue (env-pop-layer continue-env)))))
      (env-pop-layer (interpret-stmt-list (cdr stmt) (env-push-layer env) return pop-break pop-continue)))))

(define interpret-while
  (lambda (stmt env return)
    (call/cc (lambda (break)
	       (letrec ((loop (lambda (condition body env)
				(cond
				 ((eq? (interpret-value condition env) 'true) 
				  (loop condition body (call/cc (lambda (continue)
								  (interpret-stmt body (interpret-sidefx condition env) return break continue)))))
				 (else (interpret-sidefx condition env))))))
		 (loop (cadr stmt) (caddr stmt) env))))))
											    
(define interpret-assign
  (lambda (stmt env)
    (env-update (LHS stmt) (interpret-value (RHS stmt) env) (interpret-sidefx (RHS stmt) env))))
  
(define interpret-var
  (lambda (stmt env)
    (cond
     ((in-layer? (LHS stmt) (top-layer env)) (error "Error: Cant redeclare variables"))
     ((null? (cddr stmt)) (env-bind (LHS stmt) 'NEWVAR env))
     (else (env-bind (LHS stmt) (interpret-value (RHS stmt) env) (interpret-sidefx (RHS stmt) env))))))
  
(define interpret-return
  (lambda (stmt env)
    (cond
     ((and (pair? (cadr stmt)) (eq? 'funcall (caadr stmt))) (env-bind 'return (interpret-func-call (cadadr stmt) (cddadr stmt) env) env))
    (else (env-bind 'return (interpret-value (LHS stmt) env) (interpret-sidefx (LHS stmt) env))))))
  
(define interpret-if
  (lambda (stmt env return break continue)
    (cond
     ((eq? (interpret-value (cadr stmt) env) 'true) 
      (interpret-stmt (caddr stmt) (interpret-sidefx (cadr stmt) env) return break continue))
     ((interpret-else? stmt) 
      (interpret-stmt (cadddr stmt) (interpret-sidefx (cadr stmt) env) return break continue))
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
     ((eq? 'funcall (operator stmt)) (interpret-func-call (cadr stmt) (cddr stmt) env))
     ((eq? '= (operator stmt)) (interpret-value (car (cddr stmt)) env))
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
       ((op (interpret-value (operand1 stmt) env) (interpret-value (operand2 stmt) (interpret-sidefx (operand1 stmt) env))) 'true)
       (else 'false)))))
	    

(define interpret-binary
  (lambda (op)
    (lambda (stmt env)
      (op (interpret-value (operand1 stmt) env)
	  (interpret-value (operand2 stmt) (interpret-sidefx (operand1 stmt) env))))))

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

(define member?
  (lambda (a l)
    (cond
     ((null? l) #f)
     ((eq? (car l) a) #t)
     (else (member? a (cdr l))))))

(define undef-return
  (lambda (stmt)
    (error "Return cannot be used in this context")))

(define undef-break
  (lambda (stmt)
    (error "Break cannot be used in this context")))

(define undef-continue
  (lambda (stmt)
    (error "Continue cannot be used in this context")))
