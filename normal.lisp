;;; -*- mode:lisp; coding:utf-8 -*-

;;; This file is part of the aimacode from
;;; https://github.com/aimacode/aima-lisp
;;; Copyright (c) 2016 aimacode

;;; Some modify for JSCL and MOREN environment
;;; Copyright © 2017,2018 Vladimir Mezentsev



;;;; Convert Expressions to Normal Form (Conjunctive, Implicative or Horn)

;;; This could be done much more efficiently by using a special
;;; representation for CNF, which eliminates the explicit ANDs
;;; and ORs.  This code is meant to be informative, not efficient.

;;;; Top-Level Functions


(defun ->cnf (p &optional vars)
    "Convert a sentence p to conjunctive normal form [p 279-280]."
    ;; That is, return (and (or ...) ...) where
    ;; each of the conjuncts has all literal disjuncts.
    ;; VARS is a list of universally quantified variables that P is in scope of.
    (setf p (eliminate-implications (logic p)))
    (case (op p)
      (NOT (let ((p2 (move-not-inwards (arg1 p))))
               (if (literal-clause? p2) p2 (->cnf p2 vars))))
      (AND (conjunction (mappend (lambda (q) (conjuncts (->cnf q vars)))
                                 (args p))))
      (OR  (merge-disjuncts (mapcar (lambda (q) (->cnf q vars))
                                    (args p))))
      (t   p) ; p is atomic
      ))




(defun ->inf (p)
    "Convert a sentence p to implicative normal form [p 282]."
    (conjunction (mapcar #'cnf1->inf1 (conjuncts (->cnf p)))))

(defun ->horn (p)
    "Try to convert sentence to a Horn clause, or a conjunction of Horn clauses.
  Signal an error if this cannot be done."
    (let ((q (->inf p)))
        (when (not (every #'horn-clause? (conjuncts q)))
            (error "~A, converted to ~A, is not a Horn clause." p q))
        q))

(defun logic (sentence)
    "Canonicalize a sentence not implemented"
    (cond ((stringp sentence) (error "Canonicalize isnt release ~a" sentence))
          (t sentence)))



;;;; Auxiliary Functions

(defun cnf1->inf1 (p)
    ;; P is of the form (or (not a) (not b) ... c d ...)
    ;; Convert to: (=> (and a b ...) (or c d ...))
    ;; where a,b,c,d ... are positive atomic clauses
    (let ((lhs (mapcar #'arg1 (remove-if-not #'negative-clause? (disjuncts p))))
          (rhs (remove-if #'negative-clause? (disjuncts p))))
        `(=> ,(conjunction lhs) ,(disjunction rhs))))

(defun eliminate-implications (p)
    (if (literal-clause? p)
        p
        (case (intern (symbol-name (op p)) :horn)
          (=>  `(or ,(arg2 p) (not ,(arg1 p))))
          (<=> `(and (or ,(arg1 p) (not ,(arg2 p)))
                     (or (not ,(arg1 p)) ,(arg2 p))))
          (t
           (cons (op p) (mapcar #'eliminate-implications (args p)))))))

(defun move-not-inwards (p)
    "Given P, return ~P, but with the negation moved as far in as possible."
    (case (intern (symbol-name (op p)) :horn)
      (TRUE 'false)
      (FALSE 'true)
      (NOT (arg1 p))
      (AND (disjunction (mapcar #'move-not-inwards (args p))))
      (OR  (conjunction (mapcar #'move-not-inwards (args p))))
      (t (make-exp 'not p))))

(defun merge-disjuncts (disjuncts)
    "Return a CNF expression for the disjunction."
    ;; The argument is a list of disjuncts, each in CNF.
    ;; The second argument is a list of conjuncts built so far.
    (case (length disjuncts)
      (0 'false)
      (1 (first disjuncts))
      (t (conjunction
          (let ((result nil))
              (dolist (y (conjuncts (merge-disjuncts (rest disjuncts))))
                  (dolist (x (conjuncts (first disjuncts)))
                      (push (disjunction (append (disjuncts x) (disjuncts y)))
                            result)))
              (nreverse result))))))


(defun renaming? (p q &optional (bindings +no-bindings+))
    "Are p and q renamings of each other? (That is, expressions that differ
  only in variable names?)"
    (cond ((eql bindings +fail+) +fail+)
          ((equal p q) bindings)
          ((and (consp p) (consp q))
           (renaming? (rest p) (rest q)
                      (renaming? (first p) (first q) bindings)))
          ((not (and (variable? p) (variable? q)))
           +fail+)
          ;; P and Q are both variables from here on
          ((and (not (get-binding p bindings)) (not (get-binding q bindings)))
           (extend-bindings p q bindings))
          ((or (eq (lookup p bindings) q) (eq p (lookup q bindings)))
           bindings)
          (t +fail+)))

;;;; Utility Predicates and Accessors

(defconstant +logical-connectives+ '(and or not => <=>))

(defun atomic-clause? (sentence)
    "An atomic clause has no connectives or quantifiers."
    (not (member (intern (symbol-name (op sentence)) :horn) +logical-connectives+)))


(defun literal-clause? (sentence)
    "A literal is an atomic clause or a negated atomic clause."
    (or (atomic-clause? sentence)
        (and (negative-clause? sentence) (atomic-clause? (arg1 sentence)))))

(defun negative-clause? (sentence)
    "A negative clause has NOT as the operator."
    (eq (intern (symbol-name (op sentence)) :horn) 'not))


(defun horn-clause? (sentence)
    "A Horn clause (in INF) is an implication with atoms on the left and one
  atom on the right."
    (and (eq (intern (symbol-name (op sentence)) :horn) '=>)
         (every #'atomic-clause? (conjuncts (arg1 sentence)))
         (atomic-clause? (arg2 sentence))))

(defun conjuncts (sentence)
    "Return a list of the conjuncts in this sentence."
    (cond ((eq (intern (symbol-name (op sentence)) :horn) 'and) (args sentence))
          ((symbolp sentence)
           (if (eq (intern (symbol-name sentence) :horn) 'true) nil))
          (t (list sentence))))

(defun disjuncts (sentence)
    "Return a list of the disjuncts in this sentence."
    (cond ((eq (intern (symbol-name (op sentence)) :horn) 'or) (args sentence))
          ((symbolp sentence)
           (eq (intern (symbol-name sentence) :horn) 'false) nil)
          (t (list sentence))))

(defun conjunction (args)
    "Form a conjunction with these args."
    (case (length args)
      (0 'true)
      (1 (first args))
      (t (cons 'and args))))

(defun disjunction (args)
    "Form a disjunction with these args."
    (case (length args)
      (0 'false)
      (1 (first args))
      (t (cons 'or args))))

;;; EOF
