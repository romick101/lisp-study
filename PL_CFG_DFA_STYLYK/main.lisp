(defun load-files ()
  (load "/home/romick101/lisp/PL_CFG_DFA_STYLYK/LP.lisp")
  (load "/home/romick101/lisp/PL_CFG_DFA_STYLYK/CFG.lisp")
  (load "/home/romick101/lisp/PL_CFG_DFA_STYLYK/DFA.lisp")
  (load "/home/romick101/lisp/PL_CFG_DFA_STYLYK/CT4_Stylyk.lisp"))

(defun main ()
  (load-files)
  (test-building "/home/romick101/lisp/PL_CFG_DFA_STYLYK/res/pure_CFG.dot")
  (test-liveness "/home/romick101/lisp/PL_CFG_DFA_STYLYK/res/CFG_LIVENESS.dot") ;; not correct
  (test-RD "/home/romick101/lisp/PL_CFG_DFA_STYLYK/res/CFG_RD.dot")
  (test-altering "/home/romick101/lisp/PL_CFG_DFA_STYLYK/res/before-prop.dot"
		 "/home/romick101/lisp/PL_CFG_DFA_STYLYK/res/after-prop.dot"))
