#| Matrix Algebra Suite in Scheme |#

(define (!= x y) (not (= x y)))

(define (add-vectors v1 v2)
  (cond
    ((not (= (length v1) (length v2)))
     	(display "Vectors must be the same length to add"))
    ((null? v1) '())
    (else
      (cons (+ (car v1) (car v2)) (add-vectors (cdr v1) (cdr v2))))))

(define (subtract-vectors v1 v2)
  (cond
    ((not (= (length v1) (length v2)))
     	(display "Vectors must be the same length to subtract"))
    ((null? v1) '())
    (else
      (cons (- (car v1) (car v2)) (subtract-vectors (cdr v1) (cdr v2))))))

(define (scale-vector v scalar)
  (map (lambda (x) (* scalar x)) v))

(define (is-valid-matrix? m)
  (apply = (map length m)))

(define (get-dimensions-matrix-fast m)
  (if (null? m) 
    '() 
    (list (length m) (length (car m)))))

(define (get-dimensions-matrix-safe m)
  (cond
    ((not (is-valid-matrix? m))		(display "Invalid Matrix"))
    ((null? m) 				'())
    ((list (length m) (length (car m))))))

(define (same-dimensions-matrices? m1 m2) 
  (equal? (get-dimensions-matrix-safe m1) (get-dimensions-matrix-safe m2)))

(define (add-matrices m1 m2)
  (define (recurse m1 m2)
    (if (null? m1)
      '()
      (cons (add-vectors (car m1) (car m2)) (recurse (cdr m1) (cdr m2)))))
  (if (same-dimensions-matrices? m1 m2)
    (recurse m1 m2)
    (display "Matrices must be the same dimensions to add")))

(define (subtract-matrices m1 m2)
  (define (recurse m1 m2)
    (if (null? m1)
      '()
      (cons (subtract-vectors (car m1) (car m2)) (recurse (cdr m1) (cdr m2)))))
  (if (same-dimensions-matrices? m1 m2)
    (recurse m1 m2)
    (display "Matrices must be the same dimensions to add")))

(define (vector*vector v1 v2)
  (apply + (map * v1 v2)))

(define (matrix*vector m v) 
  (if (null? m) 
    '()
    (cons (vector*vector (car m) v) (matrix*vector (cdr m) v))))

(define (matrix*matrix m1 m2)
  (let ((columns (transpose m2)))
    (map (lambda (row) (matrix*vector columns row)) m1)))
(define (transpose M)
  (apply map list M))

#| Tests |#
(define expected '( (8 12 14) (10 20 30) (9880 9880 9880) (1 2 3) (9 15 15 5) (0 -1 1 -2) (1 2 3) (-1 -2 -3) (6 -3) #t #t #f (4 3) () ((58 64) (139 154))))
(define results (list
  (scale-vector '(8 12 14) 1) 
  (scale-vector '(1 2 3) 10) 
  (scale-vector '(8 8 8) 1235)
  (add-vectors '(1 2 3) '(0 0 0))
  (add-vectors '(8 14 13 2) '(1 1 2 3))
  (add-vectors '(0 0 3 1) '(0 -1 -2 -3))
  (subtract-vectors '(1 2 3) '(0 0 0))
  (subtract-vectors '(0 0 0) '(1 2 3))
  (subtract-vectors '(8 4) '(2 7))
  (is-valid-matrix? '((1 2 3) (4 5 6)))
  (is-valid-matrix? '((1 2 3)))
  (is-valid-matrix? '((1 2 3) (1 2)))
  (get-dimensions-matrix-fast '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
  (get-dimensions-matrix-fast '())
  (matrix*matrix '((1 2 3) (4 5 6)) '((7 8) (9 10) (11 12)) )
  ))

(if (equal? expected results)
  (begin (newline) (display "All tests passed.") (newline))
  (begin (newline) (display "Some tests failed.")
	 (newline) (display results)
	 (newline) (display expected)))
