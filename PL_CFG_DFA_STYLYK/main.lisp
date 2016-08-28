(defun load-files ()
  (load "/home/romick101/lisp/PL_CFG_DFA/LP.lisp")
  (load "/home/romick101/lisp/PL_CFG_DFA/CFG.lisp")
  (load "/home/romick101/lisp/PL_CFG_DFA/DFA.lisp")
  (load "/home/romick101/lisp/PL_CFG_DFA/CT4_Stylyk.lisp"))

(defun main ()
  (load-files)
  (test-building "/home/romick101/lisp/PL_CFG_DFA/res/pure_CFG.dot")
  (test-liveness "/home/romick101/lisp/PL_CFG_DFA/res/CFG_LIVENESS.dot") ;; not correct
  (test-RD "/home/romick101/lisp/PL_CFG_DFA/res/CFG_RD.dot")
  (test-altering "/home/romick101/lisp/PL_CFG_DFA/res/before-prop.dot" "/home/romick101/lisp/PL_CFG_DFA/res/after-prop.dot"))
