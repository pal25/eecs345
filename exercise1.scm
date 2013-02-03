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
     ((null? l) '())
     ((null? (cdr l)) (cons (car l) '()))
     (else (cons (car l) (cons (nestlist (cdr l)) '()))))))

(define deepcons
  (lambda (a l)
    (cond
     ((null? l) (cons a '()))
     ((list? (car l)) (cons (deepcons a (car l)) (cdr l)))
     (else (cons a l)))))

(define nestlistfront
  (lambda (l)
    (cond
     ((null? l) '())
     ((null? (cdr l)) (deepcons (car l) '()))
     (else (deepcons (deepcons (car l) '()) (nestlistfront (cdr l)))))))

(define numparens* ;Needs work, not counting right
  (lambda (l)
    (cond
     ((null? l) '1)
     ((null? (car l)) (+ 1 (numparens* (cdr l))))
     ((pair? (car l)) (+ (+ 1 (numparens* (car l))) (numparens* (cdr l))))
     (else (numparens* (cdr l))))))

(define dup* 
  (lambda (l)
    (cond
     ((null? l) '())
     ((pair? (car l)) (cons (dup* (car l)) (cons (dup* (car l)) (dup* (cdr l)))))
     (else (cons (car l) (cons (car l) (dup* (cdr l))))))))

(define myremove*
  (lambda (cmp? a l)
    (cond
     ((null? l) '())
     ((cmp? a (car l)) (myremove* cmp? a (cdr l)))
     ((pair? (car l)) (cons (myremove* cmp? a (car l)) (myremove* cmp? a (cdr l))))
     (else (cons (car l) (myremove* cmp? a (cdr l)))))))

(define listeq?
  (lambda (l1 l2)
    (cond
     ((and (null? l1) (null? l2)) #t)
     ((or (null? l2) (null? l2)) #f)
     ((eq? (car l1) (car l2)) (listeq? (cdr l1) (cdr l2)))
     ((and (pair? (car l1)) (pair? (car l2))) (and (listeq? (car l2) (car l2)) (listeq? (cdr l1) (cdr l2))))
     (else #f))))

(define removedups*
  (lambda (l)
    (cond
     ((null? l) '())
     ((pair? (car l)) (cons (removedups* (car l)) (removedups* (cdr l))))
     ((and (pair? (cdr l)) (eq? (car l) (car (cdr l)))) (removedups* (cdr l)))
     (else (cons (car l) (removedups* (cdr l)))))))

(define rmlistdups
  (lambda (a l)
    (cond
     ((null? l) '())
     ((eq? a (car l)) (rmlistdups a (cdr l)))
     (else (cons (car l) (rmlistdups a (cdr l)))))))

(define removedups** ;Not close...
  (lambda (l)
    (cond
     ((null? l) '())
     ((pair? (car l)) 

; PART OF TRANSPOSE
; cars: Takes a list of lists and returns the first element of each sublist
(define cars
  (lambda (l)
    (cond
     ((null? l) '())
     ((null? (car l)) '())
     (else (cons (car (car l)) (cars (cdr l)))))))

; PART OF TRANSPOSE
; cdrs: Takes a list of lists and returns the cdrs of each list 
(define cdrs
  (lambda (l)
    (cond
     ((null? l) '())
     ((null? (car l)) '())
     (else (cons (cdr (car l)) (cdrs (cdr l)))))))

(define transpose ; Close, () at the end is weird...
  (lambda (l)
    (cond
     ((null? l) '())
     ((null? (car l)) '())
     (else (cons (cars l) (transpose (cdrs l)))))))