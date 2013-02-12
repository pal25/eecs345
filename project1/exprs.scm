(define expression?
  (lambda (expr)
    (cond
     ((null? expr) #f)
     ((number? expr) #t)
     ((not (pair? expr)) #f)
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
  (lambda (expr)
    (cond
     ((number? expr) expr)
     ((null? (cdr expr)) (value (car expr)))
     ;((not? (pair? expr)) (lookup expr enviroment))
     ((eq? '+ (operator expr)) (+ (value (operand1 (expr))) 
				  (value (operand2 (expr)))))
     ((eq? '- (operator expr)) (- (value (operand1 (expr)))
				  (value (operand2 (expr)))))
     ((eq? '- (operator expr)) (* (value (operand1 (expr))) 
				  (value (operand2 (expr)))))
     ((eq? '/ (operator expr)) (quotient (value (operand1 (expr)))
					 (value (operand2 (expr)))))
     ((eq? '% (operator expr)) (remainder (value (operand1 (expr))) 
					  (value (operand2 (expr))))))))


(define operator
  (lambda (expr)
    (car (cdr expr))))

(define operand1
  (lambda (expr)
    (car expr)))

(define operand2
  (lambda (expr)
    (car (cdr (cdr expr)))))