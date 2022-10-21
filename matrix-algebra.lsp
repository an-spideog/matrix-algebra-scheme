; Matrix Algebra Suite in Scheme

;; I should perhaps make a type distinction between <- TODO
;; a vector: [1 2 3] and a 1 dimensional matrix [[1 2 3]]
;; for functions
;; the current reversing method is more memory efficient but less steps efficient

(define exampleVector1 '(1 2 3))
(define exampleVector2 '(3 3 3))
(define exampleScalar 3)
(define exampleMatrix1 '((1 2 3))) ; [[1 2 3]] : 1 * 3
(define exampleMatrix2 '((1) (2) (3)))
; [[1]
;  [2]
;  [3]]

(define (add-vectors v1 v2)
  (reverse (add-vectors-tool v1 v2 '())))

(define (add-vectors-tool v1 v2 out)
  ;(display " v1: ") (display v1) (display " v2: ") (display v2) (display " out: ") (display out)
  (cond
    ( (not (= (length v1) (length v2)) ) (display "Vectors must be the same length to add"))
    ( (and (null? v1) (null? v2)) out)
    (else
        (display v1) (display " ") (display v2)
        (add-vectors-tool (cdr v1) (cdr v2) (cons (+ (car v1) (car v2)) out)))))

(define (subtract-vectors v1 v2)
  (reverse (subtract-vectors-tool v1 v2 '())))

(define (subtract-vectors-tool v1 v2 out)
  (cond
    ( (not (= (length v1) (length v2))) (display "Vectors must be the same length to add"))
    ( (and (null? v1) (null? v2)) out)
    (else
        (display v1) (display " ") (display v2)
        (add-vectors-tool (cdr v1) (cdr v2) (cons (- (car v1) (car v2)) out)))))




(define (scale-tool in-vector scalar out-vector)
  (if
    (null? in-vector) out-vector
      (scale-tool (cdr in-vector) scalar (cons (* scalar (car in-vector)) out-vector  ))
  ) ; TODO: Fix this list inversion
)
(define (scale vector scalar)
  (reverse-list (scale-tool vector scalar '())))

;; O(n) steps
(define (reverse-list L)
  (reverse-list-tool L '()))


(define (reverse-list-tool input-list output-list)
  (if
    (null? input-list) output-list
      (reverse-list-tool (cdr input-list) (cons (car input-list) output-list))))


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
