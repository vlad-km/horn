;;; -*- mode:lisp; coding:utf-8 -*-

;;; This file is part of the aimacode from
;;; https://github.com/aimacode/aima-lisp
;;; Copyright (c) 2016 aimacode

;;; Some modify for JSCL and MOREN environment
;;; Copyright (C) 2017,2018 Vladimir Mezentsev


;;;; Basic utility functions and macros, used throughout the code.

;;;; List Utilities

(defun length>1 (list)
    "Is this a list of 2 or more elements?"
    (and (consp list) (cdr list)))

(defun length=1 (list)
    "Is this a list of exactly one element?"
    (and (consp list) (null (cdr list))))

(defun random-element (list)
    "Return some element of the list, chosen at random."
    (nth (random (length list)) list))

(defun mappend (fn &rest lists)
    "Apply fn to respective elements of list(s), and append results."
    (reduce #'append (apply #'mapcar fn lists) :from-end t))

(defun starts-with (list element)
    "Is this a list that starts with the given element?"
    (and (consp list) (eq (first list) element)))

(defun last1 (list)
    "Return the last element of a list."
    (first (last list)))

(defun left-rotate (list)
    "Move the first element to the end of the list."
    (append (rest list) (list (first list))))

(defun right-rotate (list)
    "Move the last element to the front of the list."
    (append (last list) (butlast list)))

(defun transpose (list-of-lists)
    "Transpose a matrix represented as a list of lists.
  Example: (transpose '((a b c) (d e f))) => ((a d) (b e) (c f))."
    (apply #'mapcar #'list list-of-lists))

(defun reuse-cons (x y x-y)
    "Return (cons x y), or reuse x-y if it is equal to (cons x y)"
    (if (and (eql x (car x-y)) (eql y (cdr x-y)))
        x-y
        (cons x y)))

(defun make-exp (op &rest args) (cons op args))
(defun op (exp) "Operator of an expression" (if (listp exp) (first exp) exp))
(defun args (exp) "Arguments of an expression" (if (listp exp) (rest exp) nil))
(defun arg1 (exp) "First argument" (first (args exp)))
(defun arg2 (exp) "Second argument" (second (args exp)))



(defun prefix->infix (exp)
    "Convert a fully parenthesized prefix expression into infix notation."
    (cond ((atom exp) exp)
          ((length=1 (args exp)) exp)
          (t (insert-between (op exp) (mapcar #'prefix->infix (args exp))))))

(defun insert-between (item list)
    "Insert item between every element of list."
    (if (or (null list) (length=1 list))
        list
        (list* (first list) item (insert-between item (rest list)))))


(defun true (&rest args) "Always return true."  t)

(defun false (&rest args) "Always return false."  nil)

;;;; Utilities for strings and symbols and printing

(defun stringify (exp)
    "Coerce argument to a string."
    (cond ((stringp exp) exp)
          ((symbolp exp) (symbol-name exp))
          (t (format nil "~A" exp))))


(defun concat-symbol (&rest args)
    "Concatenate the args into one string, and turn that into a symbol."
    (intern (apply 'jscl::concat
                   (mapcar
                    (lambda (x)
                        (if (stringp x)
                            x
                            (if (symbolp x) (symbol-name x)
                                x))) args))))



;;; EOF
