; Matrix Algebra Suite in Scheme

;; TODO: get rid of the reversing thing, it's less efficient overall

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
  (cond
    ((not (same-dimensions? m1 m2)) (display "Matrices must be the same dimensions to add")) ; TODO: Error detail | 
    ;; could make this a separate if so as not to test it every time
    ((null? m1) '())
    (else
      (cons (add-vectors (car m1) (car m2)) (add-matrices (cdr m1) (cdr m2))))))
;; Tests
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
