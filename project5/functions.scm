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
	      (func-def (env-lookup func-name env))
	      (func-values (cddr stmt))
	      (func-formal (car func-def))
	      (func-body (cadr func-def))
	      (func-env ((caddr func-def) env))
	      (current-env (func-bind-values func-values func-formal (env-push-layer func-env) cls inst)))
	 (interpret-stmt-list func-body current-env new-return break continue cls inst))))))

(define func-bind-values
  (lambda (func-values func-formal env cls inst)
    (cond
     ((null? func-formal) env)
     ((eq? (car func-formal) '&) (env-bind-box (cadr func-formal) 
					       (env-lookup-extra (car func-values) env top-val-box) 
					       (func-bind-values (cdr func-values) (cddr func-formal) env cls inst)))
     (else (env-bind (car func-formal) 
		     (interpret-value (car func-values) env cls inst)
		     (func-bind-values (cdr func-values) (cdr func-formal) env cls inst))))))

;(define create-func-env
;  (lambda (formal-params values env cls inst)
;    (letrec ((add-bindings
;	            (lambda (formal values env newenv)
;		      (cond
;		       ((null? formal) newenv)
;			((eq? (car formal) '&) (add-bindings (cddr formal) (cdr values) env (env-bind-box (cadr formal) (env-lookup-extra (car values) env top-val-box) newenv)))
;			(else (add-bindings (cdr formal) (cdr values) env (env-bind (car formal) (interpret-value (car values) env cls inst) newenv)))))))
;      (add-bindings formal-params values env (env-push-layer (env-global-layer env))))))
