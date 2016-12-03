;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

(defpackage #:sudoku-solver.asd
  (:use :cl :asdf))

(in-package :sudoku-solver.asd)

(defsystem sudoku-solver
  :name "Sudoku Solver"
  :author "Desmond Cheong Zhi Xi"
  :description "Solves Sudoku"
  :long-description "A common lisp implementation of Donald Knuth's Dancing Links and Algorithm X applied to solving sudoku"
  :serial t
  :components
  ((:file "packages")
   (:file "dancing-links" :depends-on ("packages"))
   (:file "sudoku-solver" :depends-on ("packages" "dancing-links"))))
