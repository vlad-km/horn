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

(defun mappend (fn &rest lists)
    "Apply fn to respective elements of list(s), and append results."
    (reduce #'append (apply #'mapcar fn lists) :from-end t))

(defun starts-with (list element)
    "Is this a list that starts with the given element?"
    (and (consp list) (eq (first list) element)))

(defun last1 (list)
    "Return the last element of a list."
    (first (last list)))

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

(defun true (&rest args) "Always return true."  t)

(defun false (&rest args) "Always return false."  nil)

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
