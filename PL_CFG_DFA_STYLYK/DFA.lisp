(defun test-RD (file)
  (let ((graph (rework-graph (mk-graph'($block
					($stmt "const a := 1;")
					($if "a"
					 ($stmt "const a := 2;"))
					($stmt "const b := 3")
					($while "b"
					 ($if "a - 1"
					  ($stmt "const a := a - 1")
					  ($break))))))))
    (reaching-definitions graph)
    (dump-graph-ex graph file)))

(defun test-liveness (file)
  (let ((graph (rework-graph (mk-graph'($block ($stmt "const a := 1;")
					($while "a;" ($stmt "const b := 2;")
					 ($while "b;" ($if "b;" ($break))
					  ($stmt "222;"))
					 ($stmt "const a := a;")
					 ($stmt "const a := 2;")))))))
    (liveness graph)
    (dump-graph-ex graph file)))


(defun dump-graph-ex (graph file)
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
						     (ex-data-string
						      (cl-graph:element vertex))
						     (ex-data-in
						      (cl-graph:element vertex))
						     (ex-data-out
						      (cl-graph:element vertex)))))))

(defun rework-graph (graph)
  (cl-graph:iterate-vertexes graph
			     (lambda (x)
			       (let ((str (data-string (cl-graph:element x))))

				 (if (not (or (equal "break" str)
					      (equal "continue" str)
					      (equal "start" str)
					      (equal "end" str)))
				     (setf (cl-graph:element x)
					   (mk-ex-data (first (rest (parser (lexer str))))
						       str))
				     (setf (cl-graph:element x)
					   (mk-ex-data str str))))))
  graph)

(defstruct ex-data
  id stmt string in out)

(defun mk-ex-data (stmt &optional string  in out)
  (make-ex-data :id (gensym)
		:stmt stmt
		:string string
		:in in
		:out out))

(defun defp (vertex)
  (when (listp (ex-data-stmt (cl-graph:element vertex)))
    (equal 'def (first (first (rest (ex-data-stmt (cl-graph:element vertex))))))))

(defmacro getdef (vertex)
  `(when (defp ,vertex)
     (first (rest (ex-data-stmt (cl-graph:element ,vertex))))))

(defmacro getid (vertex)
  `(when (defp ,vertex)
     (ex-data-id (cl-graph:element ,vertex))))

(defmacro get-ver-by-id (id)
  `(cl-graph:find-vertex-if  graph (lambda (x)
				     (equal ,id
					    (ex-data-id (cl-graph:element x))))))

(defmacro getvar (vertex)
  `(when (defp ,vertex)
     (second (first (rest (ex-data-stmt (cl-graph:element ,vertex)))))))

(defun get-all-vars (vertex)
  (when (listp (ex-data-stmt (cl-graph:element vertex)))
    (if (equal (first (first (rest (ex-data-stmt (cl-graph:element vertex)))))
	       'id)
	(rest (ex-data-stmt (cl-graph:element vertex)))
	(get-vars (rest (first (rest (ex-data-stmt (cl-graph:element vertex)))))))))

(defun get-vars (stmt)
  (when stmt
    (remove-duplicates
     (remove nil (mapcar (lambda (x)
			   (when (listp x)
			     (when (eq 'id (first x))
			       x)))
			 stmt))
     :test #'equal)))

(defun meet-rd (vertex)
  (let ((parents (cl-graph:parent-vertexes vertex)))
    (when parents
      (reduce (lambda (acc x)
		(union acc (when x x)))
	      parents
	      :key (lambda (parent)
		     (ex-data-out (cl-graph:element parent)))))))

(defun in-rd (vertex)
  (meet-rd vertex))

(defun out-rd (vertex graph)
  (union (when (gen-rd vertex)
	   (list (gen-rd vertex)))
	 (set-difference (when (in-rd vertex)
			   (in-rd vertex))
		         (kill-rd vertex graph))))

(defun gen-rd (vertex)
  (when (defp vertex)
    (getid vertex)))

(defun kill-rd (vertex graph)
  (when (defp vertex)
    (let ((killer (getvar vertex)))
      (remove-if-not (lambda (id)
		       (equal killer (second (getdef (get-ver-by-id id)))))
		     (ex-data-in (cl-graph:element vertex))))))

(defun gen-live (vertex)
  (get-all-vars vertex))

(defun kill-live (vertex)
  (when (defp vertex)
    (second (getdef vertex))))

(defun out-live (vertex)
  (let ((childs (cl-graph:child-vertexes vertex)))
    (when childs
      (reduce (lambda (acc x)
		(union acc x :test #'equal))
	      childs
	      :key (lambda (child)
		     (ex-data-in (cl-graph:element child)))))))

(defun in-live (vertex)
  (print (format nil "gen:~S~%out:~S~%kill:~S~%diff:~S~%union:~S~%"
		 (gen-live vertex)
		 (out-live vertex)
		 (list (kill-live vertex))
  		 (set-difference (when (out-live vertex)
  				   (out-live vertex))
  				 (list (kill-live vertex))
  				 :test #'equal)
		 (union (when (gen-live vertex)
			  (gen-live vertex))
			(set-difference (when (out-live vertex)
					  (out-live vertex))
					(when (kill-live vertex)
					  (list (kill-live vertex)))
					:test #'equal)
			:test #'equal)))
  (union (when (gen-live vertex)
	   (gen-live vertex))
	 (set-difference (when (out-live vertex)
			   (out-live vertex))
			 (when (kill-live vertex)
			   (list (kill-live vertex)))
			 :test #'equal)
	 :test #'equal))


(defun reaching-definitions (graph)
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
      (reaching-definitions graph))))

(defun liveness (graph)
  (let ((table (make-hash-table :test #'equal))
	(flag nil))
    (labels ((%traverse-back (curr)
	       (let ((tmp-out (ex-data-out (cl-graph:element curr))))
		 (unless (gethash (cl-graph:element curr) table)
		   (setf (ex-data-out (cl-graph:element curr)) (out-live curr))
		   (setf (ex-data-in (cl-graph:element curr)) (in-live curr))
		   (when (not (equalp tmp-out
				      (ex-data-out (cl-graph:element curr))))
		     (setf flag t))
		   (setf (gethash (cl-graph:element curr) table) t)
		   (mapcar (lambda (parent)
			     (%traverse-back parent))
			   (cl-graph:parent-vertexes curr))))))
      (%traverse-back (cl-graph:find-vertex-if graph (lambda (x)
						       (equal "end"
							      (ex-data-string
							       (cl-graph:element x)))))))
    (when flag
      (liveness graph))))
