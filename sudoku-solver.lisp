(compile-file "dancing-links.lisp")
(load "dancing-links")

(defpackage :sudoku-solver
  (:use :common-lisp
	:dancing-links))

(in-package :sudoku-solver)

(defvar *board* nil) ;solved sudoku board
(defvar *matrix* nil)

(let* ((size 9)
       (sqrt-size 3)
       (square (* size size))
       (answer nil)
       (blank-row (loop for i from 1 to (* 3 (* size size))
		     collect 0)))
  (loop for sqr from 1 to size
     do (let ((r (* (floor (1- sqr) sqrt-size) sqrt-size)))
	  (loop for row from (1+ r) to (+ r sqrt-size)
	     do (let ((m (* sqrt-size (mod (1- sqr) sqrt-size))))
		  (loop for col from (1+ m) to (+ m sqrt-size)
		     do (loop for num from 1 to size
			   do (let ((new-row (copy-list blank-row)))
				(setf (elt new-row (1- (+ (* (1- sqr) size) num))) 1)
				(setf (elt new-row (1- (+ square (* (1- row) size) num))) 1)
				(setf (elt new-row (1- (+ square square (* (1- col) size) num))) 1)
				(push new-row answer))))))))
  answer)

(labels ((gen (sym size)
	   (loop for i from 1 to size
	      collect (loop
			 for j from 1 to size
			 collect (list sym i j)))))
  (defun generate-matrix ()
    (multiple-value-bind (size board) (input-board)
      (let* ((sqrt-size (sqrt size))
	     (square (* size size))
	     (blank-row (loop
			   for i from 1 to (* 3 (* size size))
			   collect 0))
	     (matrix-headers (append
			      (gen 'sqr size)
			      (gen 'row size)
			      (gen 'col size)))
	     (dense-matrix nil))
	(loop
	   for sqr from 1 to size
	   do (let ((r (* (floor (1- sqr) sqrt-size) sqrt-size)))
		(loop
		   for row from (1+ r) to (+ r sqrt-size)
		   do (let ((m (* (mod (1- sqr) sqrt-size) sqrt-size)))
			(loop
			   for col from (1+ m) to (+ m sqrt-size)
			   do (let ((new-row (copy-list blank-row)))
				(setf (elt new-row (1- (+ (* (1- sqr) size) num))) 1)
				(setf (elt new-row (1- (+ square (* (1- row) size) num))) 1)
				(setf (elt new-row (1- (+ square square (* (1- col) size) num))) 1)
				(push new-row dense-matrix)))))))
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
  
