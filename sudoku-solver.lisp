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
	     (blank-row (loop for i from 1 to (* 4 (* size size)) collect 0))
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
			    (setf (elt new-row (+ (* row size) col square square square)) 1)
			    (push new-row dense-matrix)))))
	(initialize-matrix matrix-headers dense-matrix *matrix*)
	(loop
	   for row in board
	   for rowi = 1 then (1+ rowi)
	   do (loop
		 for col in row
		 for colj = 1 then (1+ colj)
		 do (unless (zerop col)
		      (let ((box (round (+ (* (floor (1- rowi) sqrt-size) sqrt-size)
					   (floor (1- colj) sqrt-size) 1))))
			(remove-from-matrix rowi colj box col)))))))))

(defun print-headers (matrix)
  (traverse-matrix column matrix right
		   (print (name column))))

(defun solve-sudoku ()
  (generate-matrix)
  (solve-matrix *matrix*))

(defun remove-from-matrix (rowi colj box val)
  (let ((hits (list (list 'row rowi val)
		    (list 'col colj val)
		    (list 'pos rowi colj)
		    (list 'sqr box val))))
    (traverse-matrix cur *matrix* right
		     (when (member (name cur) hits :test #'equal)
		       (cover-column cur)))))
  
(defun input-board ()
  (let* ((size (read))
	 (board
	  (loop
	     for row from 1 to size
	     collect (loop
			for column from 1 to size
			collect (read)))))
    (values size board)))
  
