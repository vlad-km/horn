;;; -*- mode:lisp; coding:utf-8 -*-


;;; This file is part of the aimacode from
;;; https://github.com/aimacode/aima-lisp
;;; Copyright (c) 2016 aimacode

;;; Some modify for JSCL and MOREN environment
;;; Copyright © 2017,2018 Vladimir Mezentsev


;;;; Logical Reasoning in Horn Clause Knowledge Bases

(in-package :horn)


(defun make-kb (&key (test #'eql))
    (make-hash-table :test test))

;;; Add a sentence to a Horn knowledge base.  Warn if its not a Horn sentence.
(defun tell (kb sentence)
    (dolist (clause (conjuncts (->horn sentence)))
        ;;(print (list 'tell clause 'op (op (arg2 clause))))
        ;; Each clause should be of form (=> P (Q x)); add to hash for Q
        (setf (gethash (op (arg2 clause)) kb)
              (nconc (gethash (op (arg2 clause)) kb)
                     (list clause)))))

;;; Delete each conjunct of sentence from KB.
(defun retract (kb sentence)
    (dolist (clause (conjuncts (->horn sentence)))
        ;; Each clause should be of form (=> P (Q x)); delete from hash for Q
        (setf (gethash (op (arg2 clause)) kb)
              (remove clause (gethash (op (arg2 clause)) kb)
                      :test #'renaming?))))

;;; Use backward chaining to decide if sentence is true.
(defun ask-each (kb query fn)
    (back-chain-each kb (conjuncts (->cnf query)) +no-bindings+ fn))


;;; There are three other ASK functions, defined below, that are
;;; defined in terms of ASK-EACH.  These are defined once and for all
;;; here (not for each kind of KB)."

#|
(defun ask (kb query)
    "Ask if query sentence is true; return t or nil."
    (ask-each kb (logic query)
              #'(lambda (s)  (RETURN-FROM ASK t))))
|#

;;; Ask if query sentence is true; return t or nil
(defun ask (kb query)
    (back-chain-each kb
                     (conjuncts (->cnf (logic query)))
                     +no-bindings+
                     (lambda (s)  (RETURN-FROM ASK t))))


#|
(defun ask-pattern (kb query &optional (pattern query))
    "Ask if query sentence is true; if it is, substitute bindings into pattern."
    (ask-each kb (logic query)
              #'(lambda (s) (RETURN-FROM ASK-PATTERN
                                (subst-bindings s (logic pattern))))))
|#

;;; Ask if query sentence is true; if it is, substitute bindings into pattern.
(defun ask-pattern (kb query &optional (pattern query))
    (back-chain-each kb
                     (conjuncts (->cnf (logic query)))
                     +no-bindings+
                     (lambda (s) (RETURN-FROM ASK-PATTERN
                                     (subst-bindings s (logic pattern))))))


#|
(defun ask-patterns (kb query &optional (pattern query))
    "Find all proofs for query sentence, substitute bindings into pattern
  once for each proof.  Return a list of all substituted patterns."
    (let ((pat (logic pattern))
          (results nil))
        (ask-each kb (logic query)
                  #'(lambda (s) (push (subst-bindings s pat) results)))
        (nreverse results)))
|#

;;; Find all proofs for query sentence, substitute bindings into pattern
;;;  once for each proof.  Return a list of all substituted patterns.
(defun ask-patterns (kb query &optional (pattern query))
    (let ((results nil))
        (back-chain-each kb
                         (conjuncts (->cnf (logic query)))
                         +no-bindings+
                         (lambda (s) (push (subst-bindings s (logic pattern)) results)))
        results))





#|
(defun back-chain-each (kb goals bindings fn)
    "Solve the conjunction of goals by backward chaining.
  See [p 275], but notice that this implementation is different.
  It applies fn to each answer found, and handles composition differently."
    (cond ((eq bindings +fail+) +fail+)
          ((null goals) (funcall fn bindings))
          (t (let ((goal (first goals)))
                 (case (op goal)
                   (FALSE +fail+)
                   (TRUE (back-chain-each kb (rest goals) bindings fn))
                   (= (back-chain-each kb (rest goals)
                                       (unify (arg1 goal) (arg2 goal) bindings)
                                       fn))
                   (AND (back-chain-each kb (append (conjuncts goal) goals)
                                         bindings fn))
                   (OR (for each disjunct in (disjuncts goal) do
                           (back-chain-each kb (cons disjunct goals)
                                            bindings fn)))
                   (NOT +fail+)     ; Horn clause provers can't handle NOT
                   (t ;; Look at all the clauses that could conclude the goal.
                    (for each clause in (gethash (op goal) (horn-kb-table kb)) do
                        (let ((new-clause (rename-variables clause)))
                            (back-chain-each
                             kb
                             (append (conjuncts (arg1 new-clause)) (rest goals))
                             (unify goal (arg2 new-clause) bindings)
                             fn)))))))))
|#

;;;
;;;  Solve the conjunction of goals by backward chaining.
;;;  See [aima p 275], but notice that this implementation is different.
;;;  It applies fn to each answer found, and handles composition differently.
;;;

(defun back-chain-each (kb goals bindings fn)
    ;;(print (list 'bc goals bindings))
    (cond ((eq bindings +fail+) +fail+)
          ((null goals) (funcall fn bindings))
          (t (let ((goal (first goals)))
                 ;;(print (list 'first goal 'op (op goal)))
                 ;;(print (list 'ops 'a1 (arg1 goal) 'var? (variable? (arg1 goal))  'a2 (arg2 goal)))
                 ;;(print (list 'look (lookup (arg1 goal) bindings)))
                 (case (op goal)
                   (FALSE +fail+)
                   (TRUE (back-chain-each kb
                                          (rest goals)
                                          bindings
                                          fn))
                   (= (back-chain-each kb
                                       (rest goals)
                                       (unify (arg1 goal) (arg2 goal) bindings)
                                       fn))
                   (AND (back-chain-each kb
                                         (append (conjuncts goal) goals)
                                         bindings fn))
                   (OR (dolist (disjunct (disjuncts goal))
                           (back-chain-each kb
                                            (cons disjunct goals)
                                            bindings fn)) )
                   (NOT +fail+)
                   (t
                    ;;(print (list 'other-bc goal))
                    (dolist (clause (gethash (op goal) kb))
                        ;;(print (list 'clause clause))
                        (let ((new-clause (rename-variables clause)))
                            ;;(print (list 'new-clause new-clause))
                            (back-chain-each
                             kb
                             (append (conjuncts (arg1 new-clause)) (rest goals))
                             (unify goal (arg2 new-clause) bindings)
                             fn)))  ))))))

;;; EOF
