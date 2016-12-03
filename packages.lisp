(in-package :cl-user)

(defpackage :dancing-links
  (:use :common-lisp)
  (:export :initialize-matrix
	   :solve-matrix
	   :cover-column
	   :data
	   :header-data
	   :access-name
	   :traverse-matrix))

(defpackage :sudoku-solver
  (:use :common-lisp :dancing-links))
