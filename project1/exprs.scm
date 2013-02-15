(define expression?
  (lambda (expr)
    (cond
     ((null? expr) #f)
     ((number? expr) #t)
     ((not (pair? expr)) #t)
     ((null? (cdr expr)) (expression? (car expr)))
     ((and (= (length expr) 2) (eq? '- (operator expr))) #t)
     ((not (= (length expr) 3)) #f)
     ((or (eq? '+ (operator expr)) 
	  (eq? '- (operator expr)) 
	  (eq? '* (operator expr)) 
	  (eq? '/ (operator expr)) 
	  (eq? '% (operator expr)))
      (and (expression? (operand1 expr)) 
	   (expression? (operand2 expr))))
     (else #f))))

(define conditional?
  (lambda (expr)
    (cond
     ((null? expr) #f)
     ((eq? 'true expr) #t)
     ((eq? 'false expr) #t)
     ((not (pair? expr)) #t)
     ((null? (cdr expr)) (conditional? (car expr)))
     ((not (= (length expr) 3)) #f)
     ((or (eq? '< (operator expr))
	  (eq? '> (operator expr))
	  (eq? '<= (operator expr))
	  (eq? '>= (operator expr)))
      (and (expression? (operand1 expr)) 
	   (expression? (operand2 expr))))
     (else #f))))

(define value
  (lambda (expr env)
    (cond
     ((number? expr) expr)
     ((not (pair? expr)) (env-lookup expr env))
     ((null? (cdr expr)) (value (car expr) env))
     ((and (= (length expr) 2) (eq? '- (operator expr))) (* -1 (value (operand1 expr) env)))
     ((eq? '+ (operator expr)) (+ (value (operand1 expr) env) 
				  (value (operand2 expr) env)))
     ((eq? '- (operator expr)) (- (value (operand1 expr) env)
				  (value (operand2 expr) env)))
     ((eq? '* (operator expr)) (* (value (operand1 expr) env) 
				  (value (operand2 expr) env)))
     ((eq? '/ (operator expr)) (quotient (value (operand1 expr) env)
					 (value (operand2 expr) env)))
     ((eq? '% (operator expr)) (remainder (value (operand1 expr) env) 
					  (value (operand2 expr) env))))))

(define conditional
  (lambda (expr env)
    (cond
     ((eq? 'true expr) #t)
     ((eq? 'false expr) #f)
     ((expression? expr) (value expr env))
     ((not (pair? expr)) (env-lookup expr env))
     ((null? (cdr expr)) (value (car expr) env))
     ((eq? '&& (operator expr)) (and (conditional (operand1 expr) env)
				     (conditional (operand2 expr) env)))
     ((eq? '|| (operator expr)) (or (conditional (operand1 expr) env)
				     (conditional (operand2 expr) env)))
     ((eq? '== (operator expr)) (eq? (conditional (operand1 expr) env)
				     (conditional (operand2 expr) env)))
     ((eq? '!= (operator expr)) (not (eq? (conditional (operand1 expr) env)
				     (conditional (operand2 expr) env))))
     ((eq? '< (operator expr)) (< (value (operand1 expr) env)
				     (value (operand2 expr) env)))
     ((eq? '> (operator expr)) (> (value (operand1 expr) env)
				     (value (operand2 expr) env)))
     ((eq? '<= (operator expr)) (<= (value (operand1 expr) env)
				     (value (operand2 expr) env)))
     ((eq? '>= (operator expr)) (>= (value (operand1 expr) env)
				     (value (operand2 expr) env))))))

(define operator
  (lambda (expr)
    (car expr)))

(define operand1
  (lambda (expr)
    (car (cdr expr))))

(define operand2
  (lambda (expr)
    (car (cdr (cdr expr)))))