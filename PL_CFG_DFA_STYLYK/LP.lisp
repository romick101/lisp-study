(defun test-lexer-parser ()
  (parser (lexer "2 * (4 - 5) + 1;
                  const first := 57;
                  8 * 8 * 93 - 2;
                  const second := 2 + 3;")))

(defstruct node
  data
  child-list)

(defun create-node (data child)
  (make-node :data data
	     :child-list child))

(defun add-child (tree node)
  (setf (node-child-list tree) (append (node-child-list tree)(list node))))

(defun tree-to-list (root)
  (when root
    (if (null (node-child-list root))
	(car (cons (node-data root)
		   (mapcar (lambda (child)
			     (tree-to-list child))
			   (node-child-list root))))
	(cons (node-data root)
	      (mapcar (lambda (child)
			(tree-to-list child))
		      (node-child-list root))))))
(defun test-parser ()
  (parser  '((NUM 2) MUL O-PARENTH (NUM 4) SUB (NUM 5) C-PARENTH ADD (NUM 1) SMCLN
	     CONST |first| ASGN (NUM 2) ADD (NUM 3) SMCLN
	     CONST |second| ASGN (NUM 7) DIV (NUM 9) SMCLN)))

(let* ((res nil)
       (tree (create-node 'grammar nil)))

  (defun parser (tokens)
    (setq res nil)
    (setq tree (create-node 'grammar nil))
    (grammar tokens)
    (tree-to-list (build-tree (reverse res))))

  (defun split-seq (seq delimiter)
    (let ((res-list nil)
	  (word nil))

      (mapcar (lambda (x)
		(if (not (eq x delimiter))
		    (push x word)
		    (progn
		      (when word
			(push (reverse word) res-list))
		      (setq word nil))))
	      seq)
      (when word
	(push (reverse word) res-list))
      (setq word nil)
      (reverse res-list)))

  (defun build-tree (res-tokens)
    (let ((tree (create-node 'grammar nil)))
      (mapcar (lambda (expr)
	        (add-child tree (create-node 'stmt (list (in-to-tree expr)))))
	      (split-seq res-tokens 'smcln))
      tree))

  (defun err (tokens)
    (print (format nil "ERROR: ~S" tokens))
    (print (reverse res))
    (setq res nil))

  (defun grammar (tokens)
    (stmt tokens))

  (defun stmt (tokens)
    (when tokens
      (cond
	((eq 'CONST (first tokens))
	 (let ((def-res (definition tokens)))
	   (push 'smcln res)
	   (when (rest def-res)
	     (stmt (rest def-res)))))
	(t
	 (let ((expr-res (expr tokens)))
	   (push 'smcln res)
	   (when (rest expr-res)
	     (stmt (rest expr-res))))))))

  (defun definition (tokens)
    (push 'def res)
    (when tokens
      (cond
	((eq 'CONST (first tokens))
	 (let ((var-assign (id (rest tokens))))
	   (cond
	     ((eq 'ASGN (first var-assign))
	      (expr (rest var-assign)))
	     (t
	      (err var-assign)))))
	(t
	 (err tokens)))))

  (defun id (tokens)
    (when tokens
      (push `(id ,(first tokens)) res)
      (rest tokens)))

  (defun trim-before-pars (tokens)
    (when tokens
      (reverse (cdr (member 'c-parenth (reverse tokens))))))

  (defun trim-after-pars (tokens)
    (when tokens
      (member 'c-parenth tokens)))

  (defun split-by-semi (tokens)
    (when tokens
      (print (subseq tokens 0 (position 'smcln tokens)))
      (print (subseq tokens 4))))


  (defun trim-after-semi (tokens)
    (when tokens
      (member 'smcln tokens)))

  (defun factor (tokens)
    (when tokens
      (cond
	((equalp (first tokens) 'O-PARENTH)
	 (progn
	   (push (first tokens) res)
	   (expr (trim-before-pars (rest tokens)))
	   (factor (trim-after-pars (rest tokens)))))
	((equalp (first tokens) 'C-PARENTH)
	 (push (first tokens) res)
	 (rest tokens))
	((listp (first tokens))
	 (num tokens))
	(t (id tokens)))))

  (defun expr (tokens)
    (when tokens
      (expr-list (term tokens))))

  (defun term (tokens)
    (when tokens
      (term-list (factor tokens))))

  (defun num (tokens)
    (when tokens
      (if (equalp (first (first tokens)) 'NUM)
	  (progn
	    (push (first tokens) res)
	    (rest tokens))
	  (err tokens))))

  (defun expr-list (tokens)
    (when tokens
      (cond
	((equalp (first tokens) 'ADD)
	 (push 'expr-add res)
	 (expr-list (term (rest tokens))))
	((equalp (first tokens) 'SUB)
	 (push 'expr-sub res)
	 (expr-list (term (rest tokens))))
	(t
	 tokens))))

  (defun term-list (tokens)
    (when tokens
      (cond
	((equalp (first tokens) 'mul)
	 (push 'expr-mul res)
	 (term-list (factor (rest tokens))))
	((equalp (first tokens) 'DIV)
	 (push 'expr-div res)
	 (term-list (factor (rest tokens))))
	(t
	 tokens)))))

(defun operatorp (symb)
  (or (eq 'ADD symb)
      (eq 'SUB symb)
      (eq 'MUL symb)
      (eq 'DIV symb)
      (eq 'EXPR-ADD symb)
      (eq 'EXPR-SUB symb)
      (eq 'EXPR-MUL symb)
      (eq 'EXPR-DIV symb)
      (eq 'DEF symb)))

(defun get-op-priority (op)
  (case op
    (ADD 2)
    (SUB 2)
    (MUL 3)
    (DIV 3)
    (EXPR-ADD 2)
    (EXPR-SUB 2)
    (EXPR-MUL 3)
    (EXPR-DIV 3)
    (DEF 1)))

(defun has-higher-priority (op1 op2)
  (when (and (operatorp op1)
	     (operatorp op2))
    (>= (get-op-priority op1) (get-op-priority op2))))

(defun test-in-to-post ()
  (intop
   '(o-parenth a add b c-parenth mul c sub
     o-parenth d sub e c-parenth mul o-parenth f add g c-parenth)))

(defun tree-from-postfix (expr)
  (let ((stack nil))
    (mapcar (lambda (x)
	      (if (operatorp x)
		  (push (create-node x (reverse (list (pop stack)
						      (pop stack))))
			stack)
		  (push (create-node x nil) stack)))
	    expr)
    (pop stack)))


(defun in-to-tree (in)
  (when in
    (tree-from-postfix (intop in))))

(defun test-making ()
  (tree-to-list (in-to-tree '(|var| def (num 2) add (num 3)))))

(defun intop (tokens)
  (let ((stack nil)
	(res nil))
    (loop for token in tokens
	  do (cond
	       ((operatorp token)
		(do ()
		    ((or (null stack)
			 (null (operatorp (first stack)))
			 (> (get-op-priority token) (get-op-priority (first stack)))))
		  (push (pop stack) res))
		(push token stack))
	       ((eq token 'o-parenth)
		(push token stack))
	       ((eq token 'c-parenth)
		(do ()
		    ((eq (first stack) 'o-parenth))
		  (push (pop stack) res))
		(pop stack))
	       (t (push token res))))
    (setq res (append (reverse res) stack))
    res))


;;=================================

(defun lexer (string)
  (setq string (coerce string 'list))
  (when string
    (case (process-symb (car string))
      (NUM
       (cons (get-num string)
	     (lexer (subseq string
			    (length
			     (write-to-string
			      (second
			       (get-num string))))))))
      (ALP
       (let ((id (get-id string)))
	 (cond
	   ((eq id 'const)
	    (cons id
		  (lexer (subseq string 5))))
	   (t
	    (cons id
		  (lexer (subseq string
				 (- (length
				     (write-to-string id))
				    2))))))))
      (SMCLN
       (cons 'SMCLN
	     (lexer (cdr string))))
      (SPC
       (lexer (cdr string)))
      (ASGN
       (cons 'ASGN
	     (lexer (subseq string
			    2))))
      (ERR
       nil)
      (otherwise
       (cons (process-symb (car string))
	     (lexer (cdr string)))))))


(defun get-assign (string)
  (setq string (coerce string 'list))
  (cond
    ((/= (length string) 2)
     'ERR)
    ((and (char= #\: (first string))
	  (char= #\= (second string)))
     'ASGN)
    (t
     'ERR)))

(defun get-num (string)
  `(num ,(parse-integer (coerce (process-num-test-1 string) 'string))))

(defun get-id (string)
  (let ((res (coerce (process-alpha string) 'string)))
    (cond
      ((not (alpha-char-p (first (coerce res 'list))))
       'ERR)
      ((string= res "const")
       'CONST)
      (t
       (intern res)))))


(defun process-num-test-1 (string) ;; recursive, BAD APPEND
  (setq string (coerce string 'list))
  (when (and string
	     (digit-char-p (car string)))
    (append
     (when (digit-char-p (car string))
       (list (car string)))
     (process-num-test-1 (cdr string)))))

(defun process-num-test-2 (string) ;; iterative
  (let ((num-buf nil))
    (setq string (coerce string 'list))
    (do ()
	((or (null string)
	     (not (digit-char-p (car string)))))
      (push (car string) num-buf)
      (setq string (cdr string)))
    (reverse num-buf)))

(defun process-alpha (string)
  (setq string (coerce string 'list))
  (when (and string
	     (or (digit-char-p (first string))
		 (alpha-char-p (first string))))
    (append
     (when (or (alpha-char-p (first string))
	       (digit-char-p (first string)))
       (list (first string)))
     (process-alpha (rest string)))))

(defun process-symb (symb)
  (cond
    ((or (char= #\SPACE symb)
	 (char= #\NEWLINE symb))
     'SPC)
    ((char= #\; symb)
     'SMCLN)
    ((digit-char-p symb)
     'NUM)
    ((alpha-char-p symb)
     'ALP)
    ((char= #\: symb)
     'ASGN)
    ((char= #\+ symb)
     'add)
    ((char= #\- symb)
     'sub)
    ((char= #\* symb)
     'mul)
    ((char= #\/ symb)
     'div)
    ((char= #\( symb)
     'o-parenth)
    ((char= #\) symb)
     'c-parenth)
    (T
     (print (format nil "Wrong symbol: ~S" symb))
     'err)))
