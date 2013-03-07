(define duplicate
  (lambda (a s)
    (cond
     ((eq? 0 s) '())
     (else (cons a (duplicate a (- s 1)))))))

(define duplicate-cps
  (lambda (a s k)
    (cond
     ((eq? 0 s) (k '()))
     (else (duplicate-cps a (- s 1) (lambda (v) (k (cons a v))))))))

(define removedups
  (lambda (l)
    (cond
     ((null? l) '())
     ((and (pair? (cdr l)) 
	   (eq? (car l) (car (cdr l))))
      (removedups (cdr l)))
     (else (cons (car l) (removedups (cdr l)))))))

(define removedups-cps
  (lambda (l k)
    (cond
     ((null? l) (k '()))
     ((and (pair? (cdr l))
	   (eq? (car l) (car (cdr l))))
      (removedups-cps (cdr l) k))
     (else (removedups-cps (cdr l) (lambda (v) (k (cons (car l) v))))))))

(define count*
  (lambda (a l)
    (cond
     ((null? l) 0)
     ((pair? (car l)) (+ (count* a (car l)) (count* a (cdr l))))
     ((eq? a (car l)) (+ 1 (count* a (cdr l))))
     (else (count* a (cdr l))))))

(define count-cps*
  (lambda (a l k)
    (cond
     ((null? l) (k 0))
     ((pair? (car l)) (count-cps* a (cdr l) (lambda (v) (count-cps* a (car l) (lambda (v2) (k (+ v2 v)))))))
     ((eq? a (car l)) (count-cps* a (cdr l) (lambda (v) (k (+ 1 v)))))
     (else (count-cps* a (cdr l) k)))))

(define numbersonly?
  (lambda (l)
    (cond
     ((null? l) #t)
     ((pair? (car l)) #f)
     ((number? (car l)) (numbersonly? (cdr l)))
     (else #f))))

(define numbersonly-cps?
  (lambda (l k)
    (cond
     ((null? l) (k #t))
     ((pair? (car l)) (k #f))
     ((number? (car l)) (numbersonly-cps? (cdr l) k))
     (else (k #f)))))

(define cleannumbers
  (lambda (l)
    (cond
     ((null? l) '())
     ((numbersonly? (car l)) (cons (car l) (cleannumbers (cdr l))))
     (else (cleannumbers (cdr l))))))

(define cleannumbers-cps
  (lambda (l k)
    (cond
     ((null? l) (k '()))
     ((numbersonly-cps? (car l) (lambda (v) v)) (cleannumbers-cps (cdr l) (lambda (v) (k (cons (car l) v)))))
     (else (cleannumbers-cps (cdr l) k)))))

(define merge
  (lambda (l1 l2)
    (cond
     ((null? l1) l2)
     ((null? l2) l1)
     ((<= (car l1) (car l2)) (cons (car l1) (merge (cdr l1) l2)))
     (else (cons (car l2) (merge l1 (cdr l2)))))))

(define merge-cps
  (lambda (l1 l2 k)
    (cond
     ((null? l1) (k l2))
     ((null? l2) (k l1))
     ((<= (car l1) (car l2)) (merge-cps (cdr l1) l2 (lambda (v) (k (cons (car l1) v)))))
     (else (merge-cps l1 (cdr l2) (lambda (v) (k (cons (car l2) v))))))))

(define evens
  (lambda (b l)
    (cond
     ((null? l) '())
     ((not b) (cons (car l) (evens #t (cdr l))))
     (else (evens #f (cdr l))))))

(define evens-cps
  (lambda (b l k)
    (cond
     ((null? l) (k '()))
     ((not b) (evens-cps #t (cdr l) (lambda (v) (k (cons (car l) v)))))
     (else (evens-cps #f (cdr l) k)))))

(define Mergesort
  (lambda (l)
    (cond
     ((null? l) '())
     ((null? (cdr l)) l)
     (else (merge (Mergesort (evens #t l))
		  (Mergesort (evens #f l)))))))

(define Mergesort-cps
  (lambda (l k)
    (cond
     ((null? l) (k '()))
     ((null? (cdr l)) (k l))
     (else (merge-cps (Mergesort-cps (evens-cps #t l k) k)
		      (Mergesort-cps (evens-cps #f l k) k)
		      k)))))

(define split
  (lambda (l)
    (letrec ((even-cps (lambda (b l k)
			 (cond
			  ((null? l) (k '()))
			  ((not b) (even-cps #t (cdr l) (lambda (v) (k (cons (car l) v)))))
			  (else (even-cps #f (cdr l) k))))))
      (cons (even-cps #t l (lambda (v) v))
	    (cons (even-cps #f l (lambda (v) v))
		  '())))))

(define call/cc call-with-current-continuation)

(define suffix
  (lambda (s l)
    (letrec ((suffix-check (lambda (s l k)
			     (call/cc (lambda (z)
					(cond
					 ((null? l) (k '()))
					 ((eq? s (car l)) (z (suffix s (cdr l)))) ;Recall suffix with new list
					 (else (suffix-check s 
							     (cdr l) 
							     (lambda (v) (k (cons (car l) v)))))))))))
      (suffix-check s l (lambda (v) v)))))
	 
	     
      
