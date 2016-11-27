#!/usr/bin/sbcl --script

(defclass data ()
  ((up :initarg :up :initform nil :accessor up)
   (down :initarg :down :initform nil :accessor down)
   (left :initarg :left :initform nil :accessor left)
   (right :initarg :right :initform nil :accessor right)
   (column :initarg :column :initform nil :accessor column)))

(defclass header-data (data)
  ((name :initarg :name :initform nil :accessor name)
   (size :initarg :size :initform 0 :accessor size)))

(defmethod initialize-instance :after ((x header-data) &key (columns nil))
  (setf (left x) x)
  (setf (column x) x)
  (unless (null columns)
    (loop
       for prev = x then (right prev)
       for c in columns
       do (let ((new-data (make-instance 'header-data :name c :right x)))
	    (setf (up new-data) new-data)
	    (setf (down new-data) new-data)
	    (setf (right prev) new-data)
	    (setf (left new-data) prev)
	    (setf (left x) new-data)))))

(defvar *matrix* nil)

(defun initialize-matrix (columns matrix)
  (setf *matrix* (make-instance 'header-data
				:name 'header
				:columns columns))
  (loop for row in matrix do (add-row row)))

(defun cover-column (column)
  (setf (right (left column)) (right column))
  (setf (left (right column)) (left column)) ;dance column out
  (loop ;dance row out
     for row = (down column) then (down row)
     until (equal row column)
     do (loop
	   for i = (right row) then (right i)
	   until (equal i row)
	   do (progn
		(setf (down (up i)) (down i))
		(setf (up (down i)) (up i))))))

(defun uncover-column (column)
  (setf (right (left column)) column)
  (setf (left (right column)) column) ;dance column in
  (loop ;dance row in
     for row = (up column) then (up row)
     until (equal row column)
     do (loop
	   for i = (left row) then (left i)
	   until (equal i row)
	   do (progn
		(setf (down (up i)) i)
		(setf (up (down i)) i)))))
     
;it's a sparse table, we're allowed to be sligtly inefficient for each insertion. Cut me some slack man
(defun add-row (row)
  "Adds a row of data objects to *matrix*."
  (let ((leftmost nil))
    (loop
       for i in row
       for c = (first-column *matrix*) then (right c)
       do (when (= i 1)
	    (let ((new-data (insert-data c)))
	      (if (null leftmost)
		  (progn (setf leftmost new-data)
			 (setf (left leftmost) leftmost)
			 (setf (right leftmost) leftmost))
		  (progn (setf (right new-data) leftmost)
			 (setf (left new-data) (left leftmost))
			 (setf (right (left leftmost)) new-data)
			 (setf (left leftmost) new-data))))))))

(defun first-column (matrix) (right matrix))

(defun insert-data (column)
  "Inserts a data object into a given column. Returns the new data object."
  (let ((new-data (make-instance 'data
				 :up (up column)
				 :down column
				 :column column)))
    (setf (down (up column)) new-data)
    (setf (up column) new-data)
    new-data))

(defun explore (matrix)
  (let ((cursor matrix))
    (loop for input = (read)
       until (equal input 'q)
       do (progn (case input
		   (a (setf cursor (left cursor)))
		   (s (setf cursor (down cursor)))
		   (d (setf cursor (right cursor)))
		   (w (setf cursor (up cursor))))
		 (format t "~a~%" (name (column cursor)))))))

