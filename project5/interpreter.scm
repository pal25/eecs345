(load "classParser.scm")
(load "environment.scm")
(load "functions.scm")
(load "classes.scm")

(define call/cc call-with-current-continuation)

(define interpret
  (lambda (filename class)
    ;(interpret-func-call '(funcall main) 
			 ;(interpret-global-stmt-list (parser filename) newenv)
			 ;undef-return undef-break undef-continue
			 ;identity undef-inst)))
    (interpret-func-call '(funcall main) 
			 (interpret-class-declare-list (parser filename) newenv)
			 undef-return undef-break undef-continue
			 (string->symbol class) undef-inst)))

(define interpret-global-stmt-list
  (lambda (parsetree env)
    (cond
     ((null? parsetree) env)
     (else (interpret-global-stmt-list (cdr parsetree) (interpret-global-stmt (car parsetree) env))))))

(define interpret-global-stmt
  (lambda (stmt env)
    (cond
     ((eq? 'function (car stmt)) (interpret-func-declare stmt env))
     (else (interpret-stmt stmt env undef-return undef-break undef-continue identity identity)))))

(define interpret-stmt-list
  (lambda (parsetree env return break continue cls inst)
    (cond
     ((null? parsetree) env)
     (else (interpret-stmt-list (cdr parsetree) 
				(interpret-stmt (car parsetree) env return break continue cls inst)
				return break continue
				cls inst)))))

(define interpret-stmt
  (lambda (stmt env return break continue cls inst)
    ;(begin (display "INTERPRET STMT: ") (display stmt) (newline) (newline))
    (cond
     ((eq? '= (car stmt)) (interpret-assign stmt env cls inst))
     ((eq? 'var (car stmt)) (interpret-declare stmt env cls inst))
     ((eq? 'if (car stmt)) (interpret-if stmt env return break continue cls inst))
     ((eq? 'while (car stmt)) (interpret-while stmt env return cls inst))
     ((eq? 'break (car stmt)) (break env))
     ((eq? 'continue (car stmt)) (continue env))
     ((eq? 'begin (car stmt)) (interpret-begin stmt env return break continue cls inst))
     ((eq? 'return (car stmt)) (return (interpret-return stmt env return cls inst)))
     ((eq? 'funcall (car stmt)) (interpret-func-call stmt env return break continue cls inst))
     (else (error "Error: Not a valid statement")))))

(define interpret-begin
  (lambda (stmt env return break continue cls inst)
    (let ((pop-break (lambda (break-env) (break (env-pop-layer break-env))))
	    (pop-continue (lambda (continue-env) (continue (env-pop-layer continue-env)))))
      (env-pop-layer (interpret-stmt-list (cdr stmt) (env-push-layer env) return pop-break pop-continue cls inst)))))

(define interpret-while
  (lambda (stmt env return cls inst)
    (call/cc (lambda (break)
	       (letrec ((loop (lambda (condition body env)
				(cond
				 ((eq? (interpret-value condition env cls inst) 'true) 
				  (loop condition body (call/cc (lambda (continue)
								  (interpret-stmt body env return break continue cls inst)))))
				 (else env)))))
		 (loop (cadr stmt) (caddr stmt) env))))))
    
(define interpret-assign
  (lambda (stmt env cls inst)
    (cond
     ((null? (class-lookup (LHS stmt) cls env)) (error "Error: Variable undeclared"))
     (else (if (and (list? (RHS stmt)) (eq? '= (operator (RHS stmt))))
	       (let ((sidefx-env (interpret-assign (RHS stmt) env cls inst)))
		 ;(env-update (LHS stmt) (env-lookup (operand1 (RHS stmt)) sidefx-env) sidefx-env))
	         (class-update (LHS stmt) (class-lookup (operand1 (RHS stmt)) cls sidefx-env) cls sidefx-env))
	       (class-update (LHS stmt) (interpret-value (RHS stmt) env cls inst) cls env))))))
  
(define interpret-declare
  (lambda (stmt env cls inst)
    (cond
     ((env-declared-layer? (LHS stmt) (top-layer env)) (error "Error: Cant redeclare variables"))
     ((null? (cddr stmt)) (env-bind (LHS stmt) 'NEWVAR env))
     (else (if (and (list? (RHS stmt)) (eq? '= (operator (RHS stmt))))
	       (let ((sidefx-env (interpret-assign (RHS stmt) env cls inst)))
		 (env-bind (LHS stmt) (env-lookup (operand1 (RHS stmt)) sidefx-env) sidefx-env))
	       (env-bind (LHS stmt) (interpret-value (RHS stmt) env cls inst) env))))))
  
(define interpret-return
  (lambda (stmt env return cls inst)
    (interpret-value (LHS stmt) env cls inst)))

(define interpret-if
  (lambda (stmt env return break continue cls inst)
    (cond
     ((eq? (interpret-value (cadr stmt) env cls inst) 'true) 
      (interpret-stmt (caddr stmt) env return break continue cls inst))
     ((interpret-else? stmt) 
      (interpret-stmt (cadddr stmt) env return break continue cls inst))
     (else env))))
     
(define interpret-else?
  (lambda (stmt)
    (cond
     ((null? (cdddr stmt)) #f)
     (else #t))))

(define interpret-dot-value
  (lambda (stmt env cls inst)
    (cond
     ((eq? (LHS stmt) 'super) (class-lookup (RHS stmt) (class-parent (env-lookup cls env)) env))
     (else (class-lookup (RHS stmt) (LHS stmt) env)))))

(define interpret-dot-class
  (lambda (stmt env cls inst)
    (cond
     ((eq? (LHS stmt) 'super) (class-parent (env-lookup cls env)))
     (else (LHS stmt)))))
	  
(define interpret-value
  (lambda (stmt env cls inst)
    ;(begin (display "INTERPRET VALUE: ") (display stmt) (newline) (newline))
    (cond
     ((null? stmt) '())
     ((number? stmt) stmt)
     ((eq? stmt 'true) 'true)
     ((eq? stmt 'false) 'false)
     ((atom? stmt) (if (null? (class-lookup stmt cls env)) 
		       (error "Variable not declared")
		       (class-lookup stmt cls env)))
     ((eq? 'dot (operator stmt)) (interpret-value (interpret-dot-value stmt env cls inst) env cls inst))
     ((eq? 'new (operator stmt)) (inst-newenv (cadr stmt)))
     ((eq? 'funcall (operator stmt)) (interpret-func-call stmt env undef-return undef-break undef-continue cls inst))
     ((eq? '= (operator stmt)) (env-lookup (operand1 stmt) (interpret-assign stmt env cls inst)))
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
     ((eq? '|| (operator stmt)) ((interpret-boolean boolean-or) stmt env cls inst))
     ((eq? '&& (operator stmt)) ((interpret-boolean boolean-and) stmt env cls inst))
     ((eq? '! (operator stmt)) ((interpret-unary-boolean boolean-not) stmt env cls inst))
     (else (error "Invalid expression")))))

(define boolean-or
  (lambda (a b)
    (cond
     ((and (eq? a 'true) (eq? b 'true)) #t)
     ((and (eq? a 'true) (eq? b 'false)) #t)
     ((and (eq? a 'false) (eq? b 'true)) #t)
     ((and (eq? a 'false) (eq? b 'false)) #f))))

(define boolean-and
  (lambda (a b)
    (cond
     ((and (eq? a 'true) (eq? b 'true)) #t)
     ((and (eq? a 'true) (eq? b 'false)) #f)
     ((and (eq? a 'false) (eq? b 'true)) #f)
     ((and (eq? a 'false) (eq? b 'false)) #f))))

(define boolean-not
  (lambda (a) 
    (cond 
     ((eq? a 'true) #f) 
     ((eq? a 'false) #t))))

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
       ((op (interpret-value (operand1 stmt) env cls inst) (interpret-value (operand2 stmt) env cls inst)) 'true)
       (else 'false)))))
    
(define interpret-binary
  (lambda (op)
    (lambda (stmt env cls inst)
      (op (interpret-value (operand1 stmt) env cls inst)
	    (interpret-value (operand2 stmt) env cls inst)))))

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

(define undef-inst
  (lambda (stmt)
    (begin
      (display stmt) (newline)
      (error "Inst cannot be used in this context"))))