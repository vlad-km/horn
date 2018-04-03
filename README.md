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
    (horn:tell kb '(Father Torsten Peter))
    (horn:tell kb '(Father Peter Isabella))
    (horn:tell kb '(Father Peter Juliet))
    (horn:tell kb '(=> (mother $x $y) (parent $x $y)))
    (horn:tell kb '(=> (father $x $y) (parent $x $y)))
    (horn:tell kb '(=> (and (parent $g $p) (parent $p $c)) (grand-parent $g $c)))
    (horn:ask-patterns kb '(grand-parent $x $y)) 
;;=>
;;                  '((Grand-parent Gerda Isabella) (Grand-parent Gerda Juliet) 
;;                    (Grand-parent Torsten Isabella) (Grand-parent Torsten Juliet)))
```

## Copyright
Copyright © 2017,2018 Vladimir Mezentsev

## License
GNU General Public License v3.0

