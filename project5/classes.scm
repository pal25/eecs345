(load "environment.scm")

(define interpret-class-declare-list
  (lambda (parsetree env)
    (cond
     ((null? parsetree) env)
     (else (interpret-class-declare-list (cdr parsetree) (interpret-class-declare (car parsetree) env))))))

;; (define interpret-class-declare
;;   (lambda (class env)
;;     (let* ((name (cadr class))
;; 	   (body (cadddr class))
;; 	   (parent (if (null? (caddr class)) '() (cadr (caddr class))))
;; 	   (class-env (list empty-env empty-env empty-env parent)))
;;       (env-bind name (interpret-class-stmt-list body class-env) env))))

(define interpret-class-declare
  (lambda (class env)
    (let* ((cls-name (cadr class))
	   (cls-body (cadddr class))
	   (cls-parent (if (null? (caddr class)) '() (cadr (caddr class))))
	   (cls-env (env-bind cls-name (list empty-env empty-env empty-env cls-parent) env)))
      (interpret-class-stmt-list cls-name cls-body cls-env))))

(define interpret-class-stmt-list
  (lambda (cls-name cls-body cls-env)
    (cond
     ((null? cls-body) cls-env)
     (else (interpret-class-stmt-list cls-name (cdr cls-body) (interpret-class-stmt cls-name (car cls-body) cls-env))))))

;; (define interpret-class-stmt-list
;;   (lambda (body class-env)
;;     (cond
;;      ((null? body) class-env)
;;      (else (interpret-class-stmt-list (cdr body) (interpret-class-stmt (car body) class-env))))))

(define interpret-class-stmt
  (lambda (cls-name stmt env)
    ;(begin (display "CLASS_STMT: ") (display stmt) (newline)
	   ;(display "CLASS_NAME: ") (display cls-name) (newline)
	   ;(display "ENV: ") (display env) (newline) (newline))
    (let ((class-env (env-lookup cls-name env)))
      (cond
       ((eq? 'static-function (car stmt)) (env-update cls-name (list
								(class-varenv class-env)
								(inst-varenv class-env)
								(interpret-func-declare stmt (class-methodenv class-env))
								(class-parent class-env))
						      env))
       ((eq? 'static-var (car stmt)) (interpret-declare stmt env cls-name undef-inst))))))
				    
;; (define interpret-class-stmt
;;   (lambda (stmt class-env)
;;     (begin (display "CLASS_STMT: ") (display stmt) (newline) (newline))
;;     (cond
;;      ((eq? 'static-function (car stmt)) (list 
;; 					 (class-varenv class-env)
;; 					 (inst-varenv class-env)
;; 					 (interpret-func-declare stmt (class-methodenv class-env))
;; 					 (class-parent class-env)))
;;      ((eq? 'static-var (car stmt)) (list
;; 				    (if (eq? (operator stmt) 'funcall)
;; 					(
;; 				    (interpret-declare stmt (class-varenv class-env) 'None undef-inst)
;; 				    (inst-varenv class-env)
;; 				    (class-methodenv class-env)
;; 				    (class-parent class-env))))))

; class-newenv = (class-varenv inst-varenv methodenv parent)
(define class-newenv (list empty-env empty-env empty-env 'None))

(define class-varenv (lambda (class-env) (if (null? class-env) empty-env (car class-env))))
(define inst-varenv (lambda (class-env) (if (null? class-env) empty-env (cadr class-env))))
(define class-methodenv (lambda (class-env) (if (null? class-env) empty-env (caddr class-env))))
(define class-parent (lambda (class-env) (if (null? class-env) '() (cadddr class-env))))

(define class-lookup
  (lambda (name cls-name env)
    ;(begin (display "NAME: ") (display name) (newline) 
	   ;(display "CLS_NAME: ") (display cls-name) (newline)
	   ;(display "ENV: ") (display env) (newline) (newline))
    (let ((class-env (env-lookup cls-name env)))
      (if (null? (env-lookup name (class-varenv class-env)))
	  (if (null? (env-lookup name (class-methodenv class-env)))
	      (if (or (null? (class-parent class-env))
		      (null? (class-lookup name (class-parent class-env) env)))
		  (env-lookup name env)
		  (class-lookup name (class-parent class-env) env))
	      (env-lookup name (class-methodenv class-env)))
	  (env-lookup name (class-varenv class-env))))))

(define class-update
  (lambda (name val cls-name env)
    (let* ((class-env (env-lookup cls-name env)))
      (if (null? (env-lookup name (class-varenv class-env)))
	  (if (or (null? (class-parent class-env)) (null? (class-lookup name (class-parent class-env) env)))
	      (env-update name val env)
	      (class-update name val (class-parent class-env) env))
	  (env-update cls-name (list 
				(env-update name val (class-varenv class-env))
				(inst-varenv class-env)
				(class-methodenv class-env)
				(class-parent class-env))
		      env)))))
	   
(define class-method-lookup 
  (lambda (method class-env) 
    (env-lookup-layer method (class-methodenv class-env) top-val-box)))

(define class-var-lookup 
  (lambda (var class-env) 
    (env-lookup-layer var (class-varenv class-env) top-val-box)))

; inst-newenv = (inst-varvals type)
(define inst-newenv '(() None))