(define expression?
  (lambda (expr)
    (cond
     ((null? expr) #f)
     ((number? expr) #t)
     ((not (pair? expr)) #t)
     ((null? (cdr expr)) (expression? (car expr)))
     ((not (= (length expr) 3)) #f)
     ((or (eq? '+ (operator expr)) 
	  (eq? '- (operator expr)) 
	  (eq? '* (operator expr)) 
	  (eq? '/ (operator expr)) 
	  (eq? '% (operator expr))) 
      (and (expression? (operand1 expr)) 
	   (expression? (operand2 expr))))
     (else #f))))

(define value
  (lambda (expr env)
    (cond
     ((number? expr) expr)
     ((not (pair? expr)) (env-lookup expr env))
     ((null? (cdr expr)) (value (car expr) env))
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


(define operator
  (lambda (expr)
    (car expr)))

(define operand1
  (lambda (expr)
    (car (cdr expr))))

(define operand2
  (lambda (expr)
    (car (cdr (cdr expr)))))