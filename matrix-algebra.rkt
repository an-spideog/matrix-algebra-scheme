; Matrix Algebra Suite in Scheme

(define exampleVector1 '(1 2 3))
(define exampleVector2 '(3 3 3))
(define exampleScalar 3)

(define (!= x y) (not (= x y)))

(define (add-vectors v1 v2)
  (cond
    ((not (= (length v1) (length v2))) (display "Vectors must be the same length to add"))
    ((null? v1) '())
    (else
      (cons (+ (car v1) (car v2)) (add-vectors (cdr v1) (cdr v2))))))

(define (subtract-vectors v1 v2)
  (cond
    ((not (= (length v1) (length v2))) (display "Vectors must be the same length to subtract"))
    ((null? v1) '())
    (else
      (cons (- (car v1) (car v2)) (subtract-vectors (cdr v1) (cdr v2))))))

(define (scale v x)
  (if (null? v)
    '()
    (cons (* x (car v)) (scale (cdr v) x))))

(define exampleMatrix1 '((1 2 3))) ; [[1 2 3]] : 1 * 3
(define exampleMatrix2 '((1) (2) (3)))
; [[1]
;  [2]
;  [3]] : 3 * 1
(define exampleMatrix3 '((1 2 3) (4 5 6) (7 8 9)))


(define (is-valid-matrix? matrix)
  (cond
    ((null? matrix) #t)
    ((not (list? (car matrix))) #f)
    (else (is-valid-matrix-tool matrix (length (car matrix))))
    ))

(define (is-valid-matrix-tool matrix width)
  (cond
    ((null? matrix) #t)
    ((not (list? (car matrix))) #f)
    ((!= (length (car matrix)) width) #f)
    (else (is-valid-matrix-tool (cdr matrix) (length (car matrix))))))

(define (get-dimensions matrix)
  (if (null? matrix) '() (list (length matrix) (length (car matrix)))))
;; fast

(define (get-dimensions-safe matrix)
  (if (is-valid-matrix? matrix)
    (if (null? matrix) '() (list (length matrix) (length (car matrix))))
    (display "Invalid Matrix")
    )
  )

(define (same-dimensions? m1 m2) 
  (equal? (get-dimensions-safe m1) (get-dimensions-safe m2)))

(define (add-matrices m1 m2)
  (define (recurse m1 m2)
    (if (null? m1)
      '()
      (cons (add-vectors (car m1) (car m2)) (recurse (cdr m1) (cdr m2)))))
  (if (same-dimensions? m1 m2)
    (recurse m1 m2)
    (display "Matrices must be the same dimensions to add")))

(define (subtract-matrices m1 m2)
  (define (recurse m1 m2)
    (if (null? m1)
      '()
      (cons (subtract-vectors (car m1) (car m2)) (recurse (cdr m1) (cdr m2)))))
  (if (same-dimensions? m1 m2)
    (recurse m1 m2)
    (display "Matrices must be the same dimensions to add")))

(define (dot-product v1 v2)
  (apply + (map * v1 v2)))

;; (dot-product-mv '((1 2) (3 4)) '(5 6)) -> '(17 39)
;;[[1 2]   [[5     [[17
;; [3 4] .   6]] =   39]]

(define (dot-product-mv m v) 
  (if (null? m) 
    '()
    (cons (dot-product (car m) v) (dot-product-mv (cdr m) v))))

(define (dot-product-mm m1 m2) ;; Works! Is there a better way?
  (define (dot-product-mm-tool m1 m2)
    (cond
      ((null? m2) '())
      (else
	(cons (dot-product-mv m1 (car m2)) (dot-product-mm-tool m1 (cdr m2))))))
  (transpose (dot-product-mm-tool m1 (transpose m2))))

;; New, clean, transpose
(define (transpose M)
  (apply map list M))

;; Tests
(define expected '( (8 12 14) (10 20 30) (9880 9880 9880) (1 2 3) (9 15 15 5) (0 -1 1 -2) (1 2 3) (-1 -2 -3) (6 -3) #t #t #t #f #f (4 3) () ((58 64) (139 154))))
(define results (list
  (scale '(8 12 14) 1) ; '(8 12 14)
  (scale '(1 2 3) 10) ;  '(10 20 30)
  (scale '(8 8 8) 1235) ; '(9880 9880 9880)
  (add-vectors '(1 2 3) '(0 0 0)) ; '(1 2 3)
  (add-vectors '(8 14 13 2) '(1 1 2 3)) ; '(9 15 15 5)
  (add-vectors '(0 0 3 1) '(0 -1 -2 -3)) ; '(0 -1 1 -2)
  (subtract-vectors '(1 2 3) '(0 0 0)) ; '(1 2 3)
  (subtract-vectors '(0 0 0) '(1 2 3)) ; '(-1 -2 -3)
  (subtract-vectors '(8 4) '(2 7)) ; '(6 -3)
  (is-valid-matrix? '((1 2 3) (4 5 6))) ; #t
  (is-valid-matrix? '((1 2 3))) ; #t
  (is-valid-matrix? '()) ; #t
  (is-valid-matrix? '(1 2 3)) ; #f
  (is-valid-matrix? '((1 2 3) (1 2))) ; #f
  (get-dimensions '((1 2 3) (4 5 6) (7 8 9) (10 11 12))) ; '(4 3)
  (get-dimensions '()) ; '(0 0)
  (dot-product-mm '((1 2 3) (4 5 6)) '((7 8) (9 10) (11 12)) ) ; '((58 64) (139 154))
  ))

(cond
  ((equal? expected results) (newline) (display "All tests passed.") (newline) )
  (else (newline) (display "Some tests failed.") (newline) (display results) ))
