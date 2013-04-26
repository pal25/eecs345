(define tanspose
  (lambda (n)
    (if (or (null? m) (null? (car m)))
	m
	(cons (map car m) (transpose (map cdr m))))))

(define powerset
  (lambda (l)
    (cond
     ((null? l) '(()))
     (else (append (map (lambda (n) (cons (car l) n)) (powerset (cdr l))) (powerset (cdr l)))))))

(define fold-right
  (lambda (f i l)
    (cond
     ((null? l) i)
     (else (f (car l) (fold-right f i (cdr l)))))))

(define fold-left
  (lambda (f i l)
    (cond
     ((null? l) i)
     (else (fold-left f (f i (car l)) (cdr l))))))

(define nestlist
  (lambda (l)
    (fold-right (lambda (a l) (if (null? l) (list a) (cons a (list l)))) '() l)))

(define nestlist-front
  (lambda (l)
    (fold-left list (list (car l)) (cdr l))))

(define remove-dups
  (lambda (l)
    (fold-right (lambda (a l) (if (and (eq? a (car l)) (not (null? (car l)))) l (cons a l))) '() l)))

(define makesieve
  (lambda (sieve p)
    (lambda (k)
      (letrec ((loop (lambda (j)
		       (let ((next (sieve j)))
			 (if (zero? (remainder next p))
			     (loop (+ next 1))
			     next)))))
	(loop k)))))