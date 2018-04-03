# HORN logic from aima-lisp

JSCL implementation of logic algorithms (HORN) from Russell And Norvig's book *Artificial Intelligence - A Modern Approach.*

Folder `original` contain source files from https://github.com/aimacode/aima-lisp/tree/master/logic/algorithms

In the source code, I made some changes for JSCL and Moren environment.

This implementation is only a Horn clause (prefix form).

Initially, the author's codes are written very inefficiently and need further development. But, works - do not touch it.


## Use

```lisp
    (setq kb (horn:make-kb))
    (horn:tell kb '(Mother Gerda Peter))
    (tell kb2 '(Father Torsten Peter))
    (tell kb2 '(Father Peter Isabella))
    (tell kb2 '(Father Peter Juliet))
    (tell kb2 '(=> (mother $x $y) (parent $x $y)))
    (tell kb2 '(=> (father $x $y) (parent $x $y)))
    (tell kb2 '(=> (and (parent $g $p) (parent $p $c)) (grand-parent $g $c)))
    (ask-patterns kb2 '(grand-parent $x $y) 
;;=>
;;                  '((Grand-parent Gerda Isabella) (Grand-parent Gerda Juliet) 
;;                    (Grand-parent Torsten Isabella) (Grand-parent Torsten Juliet)))
```
