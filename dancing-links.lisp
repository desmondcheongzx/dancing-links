#!/usr/bin/sbcl --script

(defclass data ()
  ((up :initarg :up :initform nil :accessor up)
   (down :initarg :down :initform nil :accessor down)
   (left :initarg :left :initform nil :accessor left)
   (right :initarg :right :initform nil :accessor right)
   (column :initarg :column :initform nil :accessor column)
   (name :initarg :name :initform nil :accessor name)))

(defmethod initialize-instance :after ((x data) &key (columns nil))
  (unless (null columns)
    (loop
       for prev = x then (right prev)
       for c in columns
	 do (setf (right prev) (make-instance 'data :name c :right x)))))
