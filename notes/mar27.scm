(define count
  (lambda (l)
    (cond
     ((null? l) 0)
     (else (+ 1 (count (cdr l)))))))

(define count1
  ((lambda (len)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (+ 1 (len (cdr l)))))))
   ((lambda (len)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (+ 1 (len (cdr l)))))))
    ((lambda (len)
       (lambda (l)
	 (cond
	  ((null? l) 0)
	  (else (+ 1 (len (cdr l)))))))
     crash))))

(define count2
  ((lambda (mklen)
     (mklen crash))
   (lambda (f)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (+ 1 (f (cdr l)))))))))

(define count3
  ((lambda (mklen)
     (mklen (mklen crash)))
   (lambda (f)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (+ 1 (f (cdr l)))))))))

(define count4
  ((lambda (mklen)
     (mklen mklen))
   (lambda (f)
     (lambda (l)
       (cond
	((null? l) 0)
	(else (+ 1 ((f f) (cdr l)))))))))

(define myappend
  (lambda (l1 l2)
    (cond
     ((null? l1) l2)
     (else (cons (car l1) (myappend (cdr l2) l2))))))

(define myappend1
  ((lambda (fcn)
    (lambda (l1 l2)
      (cond
       ((null? l1) l2)
       (else (cons (car l1) (fcn (cdr l1) l2))))))
   ((lambda (fcn)
     (lambda (l1 l2)
       (cond
	((null? l1) l2)
	(else (cons (car l1) (fcn (cdr l1) l2))))))
    crash)))

(define myappend2
  ((lambda (mk-myappend)
     (mk-myappend mk-myappend))
   (lambda (f)
     (lambda (l1 l2)
       (cond
	((null? l1) l2)
	(else (cons (car l1) ((f f) (cdr l1) l2))))))))

(define count*
  (lambda (l)
    (cond
     ((null? l) 0)
     ((list? (car l)) (+ (count* (car l)) (count* (cdr l))))
     (else (+ 1 (count* (cdr l)))))))

(define count1*
  ((lambda (mk-count)
     (mk-count mk-count))
   (lambda (f)
     (lambda (l)
       (cond
	((null? l) 0)
	((list? (car l)) (+ ((f f) (car l)) ((f f) (cdr l))))
	(else (+ 1 ((f f) (cdr l)))))))))

(define flattn
  (lambda (l)
    (cond
     ((null? l) '())
     ((pair? (car l)) (myappend (flattn (car l)) (flattn (cdr l))))
     (else (cons (car l) (flattn (cdr l)))))))

(define flattn1
  ((lambda (mk-flat mk-append)
     (mk-flat mk-flat mk-append))
   (lambda (f g)
     (lambda (l)
       (cond
	((null? l) '())
	((list? (car l)) ((g g) ((f f g) (car l)) ((f f g) (cdr l))))
	(else (cons (car l) ((f f g) (cdr l)))))))
   (lambda (g)
     (lambda (l1 l2)
      (cond
       ((null? l1) l2)
       (else (cons (car l1) ((g g) (cdr l1) l2))))))))