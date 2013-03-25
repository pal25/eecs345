(define crash (error "Bad recursion"))

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