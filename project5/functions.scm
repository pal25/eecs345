(define interpret-func-declare
  (lambda (stmt env)
    (let* ((func-name (cadr stmt))
	   (func-formal (caddr stmt))
	   (func-body (cadddr stmt))
	   (func-closure (create-closure func-name func-formal func-body)))
      (env-bind func-name (list func-formal func-body func-closure) env))))

(define create-closure
  (lambda (func-name func-formal func-body)
    (lambda (env)
      (env-bind func-name (list func-formal func-body (create-closure func-name func-formal func-body)) env))))
		
(define interpret-func-call
  (lambda (stmt env return break continue cls inst)
    (call/cc
     (lambda (new-return)
       (let* ((func-name (cadr stmt))
	      (func-def (class-lookup func-name cls env))
	      (func-values (cddr stmt))
	      (func-formal (car func-def))
	      (func-body (cadr func-def))
	      (func-env ((caddr func-def) env))
	      (current-env (func-bind-values func-values func-formal (env-push-layer func-env) cls inst)))
	 (interpret-stmt-list func-body current-env new-return break continue cls inst))))))

(define func-bind-values
  (lambda (func-values func-formal env cls inst)
    ;(begin (display "BINDING PARAMS: ") (newline)
	   ;(display "VALUES: ") (display func-values) (newline)
	   ;(display "FORMAL: ") (display func-formal) (newline) 
	   ;(display "ENV: ") (display env) (newline) (newline))
    (cond
     ((null? func-formal) env)
     ((eq? (car func-formal) '&) (env-bind-box (cadr func-formal) 
					       (env-lookup-extra (car func-values) env top-val-box) 
					       (func-bind-values (cdr func-values) (cddr func-formal) env cls inst)))
     (else (env-bind (car func-formal) 
		     (interpret-value (car func-values) env cls inst)
		     (func-bind-values (cdr func-values) (cdr func-formal) env cls inst))))))

