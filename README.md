# Matrix Algebra Suite for Scheme
This is a small project I'm putting together to enable the use of matrix algebra operations in scheme, such as creating and adding or subtracting vectors and matrices.

## Syntax
A vector is defined simply as a list of numbers, it has 1 dimension (length).
```
(define exampleVector '(1 2 3)) ; the vector [1 2 3]
```

A matrix is defined as a list of vectors, and has 2 dimensions 
```
(define exampleMatrix1 '((1 2 3) (4 5 6))
;; [[1 2 3]
    [4 5 6]]
```
The inner vectors of the list represent rows in the matrix, from top to bottom.

## Individual Commands
Here are instructions and examples on how to use the commands currently included.

### add-matrices
```
(add-matrices '((1 2 3) (4 5 6) (7 8 9)) '((1 2 3) (4 5 6) (7 8 9)))
;; Returns:
;; '((2 4 6) (8 10 12) (14 16 18))
```
Takes two matrices and returns their sum, they must have the same dimensions otherwise an error will be caused.

### add-vectors
```
(add-vectors '(1 2 3) (1 1 1))
;; Returns
;; '(2 3 4)
```
Takes two vectors and adds them together, they must have the same length otherwise an error will be caused.

### scale
```
(scale '(1 2 3) 3)
;; -> '(3 6 9)
``` 
Takes a vector and a scalar and returns a scaled vector.

### subtract-vectors
```
(subtract-vectors '(0 0 0) '(1 2 3))
;; -> '(-1 -2 -3)
```
Takes two vectors and returns the result of subtracting the second from the first, they must have the same length otherwise an error will be caused.
