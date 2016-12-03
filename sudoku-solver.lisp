(compile-file "dancing-links.lisp")
(load "dancing-links")

(defpackage :sudoku-solver
  (:use :common-lisp
	:dancing-links))

(in-package :sudoku-solver)

(defvar *board* nil) ;solved sudoku board
(defvar *matrix* nil)

(labels ((gen (sym size)
	   (apply #'append (loop for i from 1 to size
			      collect (loop for j from 1 to size
					 collect (list sym i j))))))
  (defun generate-matrix ()
    (multiple-value-bind (size board) (input-board)
      (let* ((sqrt-size (sqrt size))
	     (square (* size size))
	     (blank-row (loop for i from 1 to (* 3 (* size size)) collect 0))
	     (matrix-headers (append
			      (gen 'sqr size)
			      (gen 'row size)
			      (gen 'col size)
			      (gen 'pos size)))
	     (dense-matrix nil))
	(loop for row upto (1- size)
	   do (loop for col upto (1- size)
		 do (loop for num upto (1- size)
		       do (let ((new-row (copy-list blank-row)))
			    (setf (elt new-row (+ (* size row) square num)) 1)
			    (setf (elt new-row (+ (* size col) square square num)) 1)
			    (setf (elt new-row (round (+ (* (+ (* (floor row sqrt-size) sqrt-size)
							       (floor col sqrt-size)) size) num))) 1)
			    (push new-row dense-matrix)))))
	(initialize-matrix matrix-headers dense-matrix *matrix*)))))
  
(defun input-board ()
  (let* ((size (read))
	 (board
	  (loop
	     for row from 1 to size
	     collect (loop
			for column from 1 to size
			collect (read)))))
    (values size board)))
  
