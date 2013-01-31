(define insert
  (lambda (a l)
    (cond
     ((null? l) (cons a l))
     ((> a (car l)) (cons (car l) (insert a (cdr l))))
     (else (cons a l)))))

(define removedups
  (lambda (l)
    (cond
     ((null? l) '())
     ((eq? (car l) (car (cdr l))) (removedups (cdr l)))
     (else (cons (car l) (removedups (cdr l)))))))

(define nestlist
  (lambda (l)
    (cond
     ((null? l) '()) ; Needs work
     ((null? (cdr l)) (cons (car l) '()))
     (else (cons (car l) (cons (nestlist (cdr l)) '()))))))

(define deepcons
  (lambda (a l)
    (cond
     ((null? l) '()) ; Need to handle '(() ()) case
     ((pair? (car l)) (cons (deepcons a (car l)) (cdr l)))
     (else (cons a l)))))

(define nestlistfront
  (lambda (l)
    (cond
     ((null? l) '())
     ((null? (cdr l)) (cons (car l) '()))
     (else (cons (cons (car l) '()) (nestlistfront (cdr l)))))))