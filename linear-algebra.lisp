(defpackage :linear-algebra
  (:documentation "Linear algebra library--not meant to be used.")
  (:use :common-lisp))

;; Utilities

(defun make-zeros-matrix (r c)
  (loop for x from 0 to (1- r)
     collect (loop for y from 0 to (1- c)
		  append '(0))))

(defun repeat (x n)
  (if (zerop n) nil
      (cons x (repeat x (1- n)))))
      
(defmacro defalias (src to)
  `(setf (fdefinition ',to) ,src))

;; Matrix accessors

(defalias #'nth get-row)

(defun get-col (j M)
  (loop for xs in M
     collecting (nth j xs)))


;; Testing validity of matricies

(defun matrix? (M)
  "Returns T or NIL if or if not a valid matrix"
  (reduce #'(lambda (xs x)
	      (and xs (eql (length (get-row 0 M))
			   (length x))))
	  M :initial-value t))

(defun matrix->width (M)
  (length (get-row 0 M)))

(defun matrix->height (M)
  (length (get-col 0 M)))

(defun multipliable? (A B)
  "Tests whether AB would be a valid operation"
  (eql (matrix->width A)
       (matrix->height B)))

;; Matrix operations

(defun dot-product (xs ys)
  (assert (eql (length xs)
	       (length ys)))
  (loop for x in xs
     for y in ys
     summing (* x y)))

(defun matrix-multiply (A B)
  "Product of matrix multiplication, AB"
  (assert (and (matrix? A) (matrix? B)))
  (let ((ret (make-zeros-matrix (matrix->height A)
				(matrix->width B))))
    (dotimes (i (matrix->height A))
      ;; iterate primarily by the rows
      (dotimes (j (matrix->width B))
	;; fill the columns of matrix
	;; X_ij = A_i: * B_:j
	;(format t "~&(get-col ~a B)" j)
	(setf (elt (elt ret i) j)
	      (dot-product (get-row i A)
			   (get-col j B)))))
    ret))

(defun matrix-multiply* (&rest Ms)
  (assert (and (every #'matrix? Ms)
	       (reduce #'(lambda (x y)
			   (cond ((null x) nil)
				 ((multipliable? x y) y)
				 (t nil)))
		       Ms)))
  (reduce #'matrix-multiply Ms))
