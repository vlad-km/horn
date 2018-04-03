;;; -*- mode:lisp; coding:utf-8 -*-



(defpackage #:horn
  (:use #:cl)
  (:export #:make-kb
           #:tell
           #:retract
           #:ask
           #:ask-each
           #:ask-pattern
           #:ask-patterns))


(in-package :horn)


;;; EOF
