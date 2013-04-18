(define call/cc call-with-current-continuation)

(define factorial
  (lambda (n)
    (if (zero? n)
	1
	(* n (factorial (- n 1))))))

(define factorial-cps
  (lambda (n k)
    (if (zero? n)
	(k 1)
	(factorial-cps (- n 1) (lambda (v) (k (* v n)))))))

(define factorial-accum
  (lambda (n a)
    (if (zero? n)
	a
	(begin
	  (display "recursion is at n = ") (display n) (newline)
	  (strange)
	  (stop 1)
	  (factorial-accum (- n 1) (* a n))))))

(define strange
  (lambda ()
    (call/cc
     (lambda (k)
       (set! storecc (cons k storecc))))))

(define storecc '())

(define stop '())
(call/cc (lambda (k) (set! stop k)))
	       