(define count*
  ((lambda (mkcount)
     (mkcount mkcount))
   (lambda (f)
     (lambda (l)
       (cond
	((null? l) 0)
	((list? (car l)) (+ ((f f) (car l)) ((f f) (cdr l))))
	(else (+ 1 ((f f) (cdr l)))))))))

(define count
  ((lambda (mkcount)
     (mkcount mkcount))
   ((lambda (len-fun)
      (lambda (f)
	(len-fun (lambda (x) ((f f) x)))))
    (lambda (len)
      (lambda (l)
	(cond
	((null? l) 0)
	(else (+ 1 (len (cdr l))))))))))

(define sum
  ((lambda (mkcount)
     (mkcount mkcount))
   ((lambda (count-fun)
      (lambda (f)
	(count-fun (lambda (x) ((f f) x)))))
    (lambda (sum)
      (lambda (l)
	(cond
	 ((null? l) 0)
	 (else (+ (car l) (sum (cdr l))))))))))

(define Y
  (lambda (frame)
    ((lambda (mk)
       (mk mk))
     (lambda (f)
       (frame (lambda (x) ((f f) x)))))))

(define sum-Y
  (Y (lambda (s)
       (lambda (l)
	 (cond 
	  ((null? l) 0)
	  (else (+ (car l) (s (cdr l)))))))))

(define factorial
  (lambda (n)
    (cond
     ((zero? n) 1)
     (else (* n (factorial (- n 1)))))))

(define factorial-Y
  (Y (lambda (fact)
       (lambda (n)
	 (cond
	  ((zero? n) 1)
	  (else (* n (fact (- n 1)))))))))

(define remove
  (lambda (a l)
    (cond
     ((null? l) '())
     ((eq? a (car l)) (remove a (cdr l)))
     (else (cons (car l) (remove a (cdr l)))))))

(define remove-curry
  (lambda (a)
    (Y (lambda (rem)
	 (lambda (l)
	   (cond
	    ((null? l) '())
	    ((eq? a (car l)) (rem a (cdr l)))
	    (else (cons (car l) (rem a (cdr l))))))))))
