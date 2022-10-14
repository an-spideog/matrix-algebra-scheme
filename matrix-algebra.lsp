; Matrix Algebra Suite in Scheme



(define exampleScalar 3)
(define exampleVector '((1 2 3))) ; [[1 2 3]] : 1 * 3
(define exampleMatrix '((1 2 3) (3 2 1)))
; [[ 1 2 3]
;  [ 3 2 1]]

(define (scale-tool in-vector scalar out-vector)
  (if
    (null? in-vector) out-vector
      (scale-tool (cdr in-vector) scalar (cons (* scalar (car in-vector)) out-vector  ))
  ) ; TODO: Fix this list inversion
)
(define (scale vector scalar)
  (list (reverse-list (scale-tool (car vector) scalar '()))))


(define (reverse-list L)
  (reverse-list-tool L '()))


(define (reverse-list-tool input-list output-list)
  (if
    (null? input-list) output-list
      (reverse-list-tool (cdr input-list) (cons (car input-list) output-list))))

(scale exampleVector exampleScalar)
