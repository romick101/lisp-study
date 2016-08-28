(defun dump-graph-alt (graph file)
  (cl-graph:graph->dot graph file
		       :edge-labeler nil
		       :vertex-labeler (lambda (vertex stream)
					 (if (or (equal "break"
							(ex-data-string
							 (cl-graph:element vertex)))
						 (equal "continue"
							(ex-data-string
							 (cl-graph:element vertex)))
						 (equal "start"
							(ex-data-string
							 (cl-graph:element vertex)))
						 (equal "end"
							(ex-data-string
							 (cl-graph:element vertex))))
					     (format stream "~(~A~)"
						     (ex-data-string
						      (cl-graph:element vertex)))
					     (format stream
						     "id:~(~A~)~%~(~A~)~%in:~(~A~)~% out:~(~A~)"
						     (ex-data-id
						      (cl-graph:element vertex))
						     (ex-data-stmt
						      (cl-graph:element vertex))
						     (ex-data-in
						      (cl-graph:element vertex))
						     (ex-data-out
						      (cl-graph:element vertex)))))))
(defun rework-expr (table expr)
  (mapcar (lambda (x)
	    (cond
	      ((gethash x table)
	       (gethash x table))
	      ((nump x)
	       (nump x))
	      ((idp x)
	       (idp x))
	      ((eq x 'def)
	       'def)
	      ((listp x)
	       (rework-expr table x))))
	  expr))


(defun try-eval (expr) ;;TODO rework
  (handler-case
      (eval expr)
    (error () expr)))

(defun nump (thing)
  (when (listp thing)
    (when (= 2 (length thing))
      (when (eq 'num (first thing))
	(second thing)))))

(defun idp (thing)
  (when (listp thing)
    (when (= 2 (length thing))
      (when (eq 'id (first thing))
	(second thing)))))

(defun test-reworking ()
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash 'expr-add table) '+)
    (setf (gethash 'expr-sub table) '-)
    (setf (gethash 'expr-mul table) '*)
    (setf (gethash 'expr-div table) '/)
    (print (rework-expr table '(expr-sub (num 10) (num 2))))
    (print (rework-expr table '(expr-mul (id b) (id a))))
    (print (rework-expr table '(expr-add (num 0) (num 2))))
    (print (rework-expr table '(expr-div (num 10) (num 2))))
    (print (rework-expr table '(expr-mul (expr-add (num 1) (id b)) (num 2))))
    (print (rework-expr table '(def (id poop) (expr-mul (num 3) (num 4)))))
    (print (rework-expr table '(def (id crap) (num 88))))))


(defun get-op-table ()
  (let ((table (make-hash-table :test #'equal)))
    (setf (gethash 'expr-add table) '+)
    (setf (gethash 'expr-sub table) '-)
    (setf (gethash 'expr-mul table) '*)
    (setf (gethash 'expr-div table) '/)
    table))

(defun get-default-graph ()
  (rework-graph (mk-graph
		 '($block
		   ($stmt "const a := 5 + 3;")
		   ($if "a + 7;"
		    ($block
		     ($stmt "const a := 2;")
		     ($stmt "const b := a - 3;")
		     ($stmt "a + b"))
		    ($stmt "const a := 3;"))
		   ($stmt "a * 3;")))))

(defun alter-graph (graph table)
  (cl-graph:iterate-vertexes graph
			     (lambda (vertex)
			       (when (listp (ex-data-stmt
					     (cl-graph:element vertex)))
				 (if (defp vertex)
				     (setf (ex-data-stmt
					    (cl-graph:element vertex))
					   (rework-expr table
							(first (rest
								(ex-data-stmt
								 (cl-graph:element vertex))))))
				     (setf (ex-data-stmt
					    (cl-graph:element vertex))
					   (rework-expr table
							(first (rest
								(ex-data-stmt
								 (cl-graph:element vertex))))))))))
  graph)


(defun test-altering (old-file new-file)
  (let ((graph (get-default-graph)))
    (dump-graph-ex graph old-file)
    (rd graph)
    (dump-graph-alt (propagate (alter-graph graph (get-op-table)))
		    new-file)))

(defun rd(graph)
  (let ((table (make-hash-table :test #'equal))
	(flag nil))
    (labels ((%traverse (curr)
	       (let ((tmp-in (ex-data-in (cl-graph:element curr))))
		 (unless (gethash (cl-graph:element curr) table)
		   (setf (ex-data-in (cl-graph:element curr)) (in-rd curr))
		   (setf (ex-data-out (cl-graph:element curr)) (out-rd curr graph))
		   (when (not (equalp tmp-in
				      (ex-data-in (cl-graph:element curr))))
		     (setf flag t))
		   (setf (gethash (cl-graph:element curr) table) t)
		   (mapcar (lambda (child)
			     (%traverse child))
			   (cl-graph:child-vertexes curr))))))
      (%traverse (first (cl-graph:graph-roots graph))))
    (when flag
      (rd graph))))

(defun filter-in (var vertex graph)
  (loop for id in (ex-data-in
		   (cl-graph:element vertex))
	when (equal var
		    (second
		     (ex-data-stmt
		      (cl-graph:element
		       (get-ver-by-id id)))))
	  collect (third
		   (ex-data-stmt
		    (cl-graph:element
		     (get-ver-by-id id))))))

(defun rework-stmt (vertex graph &optional stmt)
  (mapcar (lambda (x)
	    (cond
	      ((numberp x)
	       x)
	      ((symbolp x)
	       (if (= 1 (length (filter-in x vertex graph)))
		   (first (filter-in x vertex graph))
		   x))
	      ((listp x)
	       (rework-stmt vertex graph x))))
	  (if stmt
	      (when (listp stmt)
		stmt)
	      (when (listp (ex-data-stmt
			    (cl-graph:element vertex)))
		(ex-data-stmt
		 (cl-graph:element vertex))))))

(defun propagate (graph)
  ;; go through prepared by rd and alter graph
  ;; and see if we can replace some variables
  (let ((table (make-hash-table :test #'equal)))
    (labels ((%traverse (curr)
	       (unless (gethash (cl-graph:element curr) table)
		 (setf (ex-data-stmt (cl-graph:element curr))
		       (if (listp (ex-data-stmt (cl-graph:element curr)))
			   (if (eq 'def (first (ex-data-stmt (cl-graph:element curr))))
			       ;;TODO def processing
			       (ex-data-stmt (cl-graph:element curr))
			       (rework-stmt curr graph))
			   (ex-data-stmt (cl-graph:element curr))))
		 (setf (ex-data-stmt (cl-graph:element curr))
		       (try-eval (ex-data-stmt (cl-graph:element curr))))
		 (setf (gethash (cl-graph:element curr) table) t)
		 (mapcar (lambda (child)
			   (%traverse child))
			 (cl-graph:child-vertexes curr)))))
      (%traverse (first (cl-graph:graph-roots graph)))))
  graph)
