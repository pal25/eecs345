(load "verySimpleParser.scm")

(define interpret
  (lambda (filename)
    (lookup 'return (interpret-statement-list
		     (parser filename)
		     newenv))))

(define interpret-stmt-list
  (lambda (parsetree, env)
    (cond
     ((null? parsetree) env)
     (else (interpret-stmt-list (cdr parsetree)
				(interpret-stmt (car parsetree)
						env))))))

(define interpret-stmt
  (lambda (stmt, env)
    (cond
     ((eq? '= (car stmt))
      (interpret-assign stmt env))
     ((eq? 'var (car stmt))
      (interpret-var stmt env))
     ((eq? 'if (car stmt))
      (interpret-if stmt env))
     ((eq? 'return (car stmt))
      (interpret-return stmt env))
     ((expression? stmt)
      (value stmt)))))

