(load "classParser.scm")
(load "environment.scm")
(load "functions.scm")
(load "classes.scm")

(define call/cc call-with-current-continuation)

(define interpret
  (lambda (filename classname)
    (let* ((global-env (interpret-global-stmt-list (parser filename) newenv))
	   (class-env (env-lookup (string->symbol classname) global-env)))
      (interpret-func-call 'main '() global-env (string->symbol classname) 'None))))

(define interpret-global-stmt-list
  (lambda (parsetree env)
    (cond
     ((null? parsetree) env)
     (else (interpret-global-stmt-list (cdr parsetree) (interpret-global-stmt (car parsetree) env))))))

(define interpret-global-stmt
  (lambda (stmt env)
    (cond
     ((eq? 'class (car stmt)) (interpret-class-declare (cadr stmt) 
						       (cadddr stmt) 
						       (if (null? (caddr stmt)) 'None (cadr (caddr stmt)))
						       'None 'None env))
     (else (interpret-stmt stmt env 'None 'None undef-return undef-break undef-continue)))))

(define interpret-stmt-list
  (lambda (parsetree env cls inst return break continue)
    (cond
     ((null? parsetree) env)
     (else (interpret-stmt-list (cdr parsetree) 
				(interpret-stmt (car parsetree)	env cls inst return break continue) 
				cls inst
				return break continue)))))

(define interpret-stmt
  (lambda (stmt env cls inst return break continue)
    (cond
     ((eq? '= (car stmt)) (interpret-assign stmt env cls inst))
     ((eq? 'var (car stmt)) (interpret-var stmt env cls inst))
     ((eq? 'if (car stmt)) (interpret-if stmt env cls inst return break continue))
     ((eq? 'while (car stmt)) (interpret-while stmt env cls inst return))
     ((eq? 'break (car stmt)) (break env))
     ((eq? 'continue (car stmt)) (continue env))
     ((eq? 'begin (car stmt)) (interpret-begin stmt env cls inst return break continue))
     ((eq? 'return (car stmt)) (return (interpret-return stmt env cls inst)))
     ((eq? 'funcall (car stmt)) (interpret-func-call (cadr stmt) (cddr stmt) env cls inst))
     ((eq? 'dot (car stmt)) (interpret-dot stmt env cls inst))
     (else (error "Error: Not a valid statement")))))

(define interpret-dot
  (lambda (stmt env cls inst)
    env))

(define interpret-sidefx
  (lambda (stmt env)
    env))
     

(define interpret-begin
  (lambda (stmt env cls inst return break continue)
    (let ((pop-break (lambda (break-env) (break (env-pop-layer break-env))))
	  (pop-continue (lambda (continue-env) (continue (env-pop-layer continue-env)))))
      (env-pop-layer (interpret-stmt-list (cdr stmt) (env-push-layer env) return pop-break pop-continue)))))

(define interpret-while
  (lambda (stmt env cls inst return)
    (call/cc (lambda (break)
	       (letrec ((loop (lambda (condition body env)
				(cond
				 ((eq? (interpret-value condition env cls inst) 'true) 
				  (loop condition body (call/cc (lambda (continue)
								  (interpret-stmt body env return break continue)))))
				 (else env)))))
		 (loop (cadr stmt) (caddr stmt) env))))))
											    
(define interpret-assign
  (lambda (stmt env cls inst)
    (env-update (LHS stmt) (interpret-value (RHS stmt) env cls inst) (interpret-sidefx (RHS stmt) env))))
  
(define interpret-var
  (lambda (stmt env cls inst)
    (cond
     ((env-declared-layer? (LHS stmt) (top-layer env)) (error "Error: Cant redeclare variables"))
     ((null? (cddr stmt)) (env-bind (LHS stmt) 'NEWVAR env))
     (else (env-bind (LHS stmt) (interpret-value (RHS stmt) env cls inst) (interpret-sidefx (RHS stmt) env))))))
  
(define interpret-return
  (lambda (stmt env cls inst)
    (cond
     ((and (pair? (cadr stmt)) (eq? 'funcall (caadr stmt))) (env-bind 'return (interpret-func-call (cadadr stmt) (cddadr stmt) env) env))
    (else (env-bind 'return (interpret-value (LHS stmt) env cls inst) (interpret-sidefx (LHS stmt) env))))))
  
(define interpret-if
  (lambda (stmt env cls inst return break continue)
    (cond
     ((eq? (interpret-value (cadr stmt) env cls inst) 'true) 
      (interpret-stmt (caddr stmt) env return break continue))
     ((interpret-else? stmt) 
      (interpret-stmt (cadddr stmt) (interpret-sidefx (cadr stmt) env) return break continue))
     (else env))))
     
(define interpret-else?
  (lambda (stmt)
    (cond
     ((null? (cdddr stmt)) #f)
     (else #t))))

(define interpret-value
  (lambda (stmt env cls inst)
    (cond
     ((null? stmt) '())
     ((number? stmt) stmt)
     ((eq? stmt 'true) 'true)
     ((eq? stmt 'false) 'false)
     ((atom? stmt) (class-var-lookup stmt env cls inst))
     ((eq? 'funcall (operator stmt)) (interpret-func-call (cadr stmt) (cddr stmt) env cls inst))
     ((eq? '= (operator stmt)) (interpret-value (car (cddr stmt)) env cls inst))
     ((eq? '+ (operator stmt)) ((interpret-binary +) stmt env cls inst))
     ((eq? '- (operator stmt)) ((interpret-negative -) stmt env cls inst))
     ((eq? '* (operator stmt)) ((interpret-binary *) stmt env cls inst))
     ((eq? '/ (operator stmt)) ((interpret-binary quotient) stmt env cls inst))
     ((eq? '% (operator stmt)) ((interpret-binary remainder) stmt env cls inst))
     ((eq? '> (operator stmt)) ((interpret-boolean >) stmt env cls inst))
     ((eq? '< (operator stmt)) ((interpret-boolean <) stmt env cls inst))
     ((eq? '>= (operator stmt)) ((interpret-boolean >=) stmt env cls inst))
     ((eq? '<= (operator stmt)) ((interpret-boolean <=) stmt env cls inst))
     ((eq? '!= (operator stmt)) ((interpret-boolean (lambda (a b) (not (eq? a b)))) stmt env cls inst))
     ((eq? '== (operator stmt)) ((interpret-boolean (lambda (a b) (eq? a b))) stmt env cls inst))
     ((eq? '|| (operator stmt)) ((interpret-boolean (lambda (a b)
						      (cond
						       ((and (eq? a 'true) (eq? b 'true)) #t)
						       ((and (eq? a 'true) (eq? b 'false)) #t)
						       ((and (eq? a 'false) (eq? b 'true)) #t)
						       ((and (eq? a 'false) (eq? b 'false)) #f))))
				 stmt env cls inst))
     ((eq? '&& (operator stmt)) ((interpret-boolean (lambda (a b)
						       (cond
						       ((and (eq? a 'true) (eq? b 'true)) #t)
						       ((and (eq? a 'true) (eq? b 'false)) #f)
						       ((and (eq? a 'false) (eq? b 'true)) #f)
						       ((and (eq? a 'false) (eq? b 'false)) #f))))
				 stmt env cls inst))
     ((eq? '! (operator stmt)) ((interpret-unary-boolean (lambda (a) (cond ((eq? a 'true) #f)
									   ((eq? a 'false) #t))))
				stmt env cls inst))
     (else (error "Invalid expression")))))

(define interpret-unary-boolean
  (lambda (op)
    (lambda (stmt env cls inst)
      (cond
       ((op (interpret-value (operand1 stmt) env cls inst)) 'true)
       (else 'false)))))

(define interpret-boolean
  (lambda (op)
    (lambda (stmt env cls inst)
      (cond
       ((op (interpret-value (operand1 stmt) env cls inst) (interpret-value (operand2 stmt) (interpret-sidefx (operand1 stmt) env cls inst))) 'true)
       (else 'false)))))
	    

(define interpret-binary
  (lambda (op)
    (lambda (stmt env cls inst)
      (op (interpret-value (operand1 stmt) env cls inst)
	  (interpret-value (operand2 stmt) (interpret-sidefx (operand1 stmt) env) cls inst)))))

(define interpret-negative
  (lambda (op)
    (lambda (stmt env cls inst)
      (cond
       ((null? (cddr stmt)) (* -1 (interpret-value (operand1 stmt) env cls inst)))
       (else (op (interpret-value (operand1 stmt) env cls inst)
		 (interpret-value (operand2 stmt) env cls inst)))))))

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
