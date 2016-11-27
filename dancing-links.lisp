#!/usr/bin/sbcl --script

(defclass data ()
  ((up :initarg :up :initform nil :accessor up)
   (down :initarg :down :initform nil :accessor down)
   (left :initarg :left :initform nil :accessor left)
   (right :initarg :right :initform nil :accessor right)
   (column :initarg :column :initform nil :accessor column)
   (name :initarg :name :initform nil :accessor name)))

(defmethod initialize-instance :after ((x data) &key (columns nil))
  (setf (left x) x)
  (unless (null columns)
    (loop
       for prev = x then (right prev)
       for c in columns
       do (let ((new-data (make-instance 'data :name c :right x)))
	    (setf (up new-data) new-data)
	    (setf (down new-data) new-data)
	    (setf (right prev) new-data)
	    (setf (left new-data) prev)
	    (setf (left x) new-data)))))

(defvar *matrix* nil)

(defun add-row (row)
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

(defun first-column (matrix)
  (right matrix))

(defun insert-data (column)
  (let ((new-data (make-instance 'data
				 :up (up column)
				 :down column
				 :column column)))
    (setf (down (up column)) new-data)
    (setf (up column) new-data)
    new-data))
