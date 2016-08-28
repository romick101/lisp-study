(defun test-building (file)
  (dump-graph
   (mk-graph
    '($block
      ($stmt "const a := 17;")
      ($while "a;"
       ($if "a + 1;"
	($stmt "const a := 2 + 3;")
	($break))
       ($stmt "const b := 3 * a;"))
      ($while "b;"
       ($if "b + a;"
	($break)
	($block
	 ($stmt "const c := 16 * 93;")
	 ($if "a + b + c;"
	  ($continue)))))
      ($while "c;"
       ($stmt "const b := b + 1;")
       ($stmt "const a := a + 1;"))))
   file))

(defun dump-graph (graph file)
  (cl-graph:graph->dot graph file
		       :edge-labeler nil
		       :vertex-labeler (lambda (vertex stream)
					 (format stream "~(~A~)"
						 (data-string
						  (cl-graph:element vertex))))))


(defmacro mk-edge (vertex-1 vertex-2)
  `(cl-graph:add-edge-between-vertexes graph
				       ,vertex-1
				       ,vertex-2))
(defmacro rm-edge (vertex-1 vertex-2)
  `(cl-graph:delete-edge-between-vertexes graph ,vertex-1 ,vertex-2))

(defmacro mk-vertex (data)
  `(cl-graph:add-vertex graph ,data))

(defmacro rm-vertex (string)
  `(let* ((vertex (cl-graph:search-for-vertex graph
					      ,string
					      :key (lambda (x)
						     (data-string (cl-graph:element x)))
					      :test #'equal)))
     (cl-graph:iterate-parents vertex (lambda (parent)
					(mk-edge parent (first (cl-graph:child-vertexes vertex)))))
     (cl-graph:delete-vertex graph
			     vertex)))

(defmacro my-find-vertex (string)
  `(handler-case
       (cl-graph:search-for-vertex graph
				   ,string
				   :key (lambda (x)
					  (data-string (cl-graph:element x)))
				   :test #'equal)
     (error () nil)))

(defmacro wildcard-vertex (string)
  `(handler-case
       (cl-graph:search-for-vertex graph
				   ,string
				   :key (lambda (x)
					  (data-string (cl-graph:element x)))
				   :test (lambda (x y)
					   (print "searching")
					   (print (format nil "~S in ~S" ,string x))
					   (print (search x y))))
     (error () nil)))


(defun rm-endif (graph)
  (when (my-find-vertex "endif")
    (rm-vertex "endif")
    (rm-endif graph)))

(defun rm-endwhile (graph)
  (when (my-find-vertex "endwhile")
    (rm-vertex "endwhile")
    (rm-endwhile graph)))

(defun rm-dead (graph)
  (let ((roots (cl-graph:graph-roots graph)))
    (when (> (length roots) 1)
      (warn "Deleting unreachable code: ~S" (data-string (cl-graph:element
							  (first
							   (remove-if (lambda (root)
									(equal "start"
									       (data-string
										(cl-graph:element root))))
								      roots)))))
      (mapcar (lambda (root)
		(unless (equal "start"(data-string (cl-graph:element root)))
		  (rm-vertex (data-string (cl-graph:element root)))))
	      roots)
      (rm-dead graph))))

(defun make-pretty (graph)
  (rm-endif graph)
  (rm-endwhile graph)
  (rm-dead graph))

(defstruct data
  id
  string)

(defun mk-data (string &optional id)
  (if id
      (make-data :id id
		 :string string)
      (make-data :id (gensym)
		 :string string)))

(let* ((graph nil)
       (recent-added nil)
       (c-while nil)
       (c-end nil)
       (curr recent-added))

  (defun mk-graph (expr)
    (init)
    (build expr)
    (mk-edge recent-added (mk-data "end"))
    (make-pretty graph)
    graph)

  (defun test-ifelse ()
    (init)
    (build '($block
	     ($stmt "first")
	     ($stmt "second")
	     ($stmt "third")))
    (dump-graph graph  "/home/romick101/lisp/files/graph/cfg_test.dot"))

  (defun init ()
    (setq graph (cl-graph:make-graph 'cl-graph:dot-graph
				     :default-edge-type
				     :directed))
    (setq recent-added (cl-graph:add-vertex graph (mk-data "start")))
    (setq  curr recent-added))

  (defun build (expr)
    (when expr
      (cond
	((eq (first expr) '$block)
	 (mapcar #'build (rest expr))
	 recent-added)

	((eq (first expr) '$stmt)
	 (add-stmt expr))

	((eq (first expr) '$if)
	 (add-if expr))

	((eq (first expr) '$while)
	 (add-while expr))

	((eq (first expr) '$break)
	 (unless c-while
	   (error "BREAK outside while body"))
	 (add-stmt-str "break")
	 (mk-edge recent-added c-end)
	 recent-added)

	((eq (first expr) '$continue)
	 (unless c-while
	   (error "CONTINUE outside while body"))
	 (add-stmt-str "continue")
	 (mk-edge recent-added c-while)
	 recent-added))))

  (defun add-stmt (expr)
    (let ((stmt nil))
      (setq stmt (mk-vertex (mk-data (second expr))))
      (unless (or (equal
		   "break"
		   (data-string (cl-graph:element recent-added)))
		  (equal
		   "continue"
		   (data-string (cl-graph:element recent-added))))
	(mk-edge recent-added stmt))
      (setq recent-added stmt)))

  (defun add-stmt-str (string)
    (let ((stmt nil))
      (setq stmt (mk-vertex (mk-data string)))
      (mk-edge recent-added stmt)
      (setq recent-added stmt)))

  (defun add-if (expr)
    (let (($if nil)
	  (then nil)
	  (else nil)
	  (end nil))
      (cond
	((= (length expr) 4)
	 (setq $if (mk-vertex (mk-data(second expr))))
	 (mk-edge recent-added $if)
	 (setq recent-added $if)

	 (setq then (build (third expr))) ;; then
	 (setq recent-added $if)

	 (setq else (build (fourth expr))) ;; else
	 (setq recent-added $if)

	 (setq end (mk-vertex (mk-data "endif" 1)))
	 (unless (or (equal
		      "break"
		      (data-string (cl-graph:element then)))
		     (equal
		      "continue"
		      (data-string (cl-graph:element then))))
	   (mk-edge then end))
	 (unless (or (equal
		      "break"
		      (data-string (cl-graph:element else)))
		     (equal
		      "continue"
		      (data-string (cl-graph:element else))))
	   (mk-edge else end))

	 (setq recent-added end))
	((= (length expr) 3)
	 (setq $if (mk-vertex (mk-data  (second expr))))
	 (mk-edge recent-added $if)
	 (setq recent-added $if)

	 (setq then (build (third expr))) ;; then
	 (setq recent-added $if)

	 (setq end (mk-vertex (mk-data "endif" 1)))
	 (unless (or (equal
		      "break"
		      (data-string (cl-graph:element then)))
		     (equal
		      "continue"
		      (data-string (cl-graph:element then))))
	   (mk-edge then end))
	 (mk-edge recent-added end)

	 (setq recent-added end))
	(t
	 (error "Wrong if body: ~S" (second expr))))))


  (defun add-while (expr)
    (let (($while nil)
	  (body nil)
	  (end nil))
      (setq $while (mk-vertex (mk-data (second expr))))
      (mk-edge recent-added $while)
      (setq recent-added $while)
      (setq c-while $while)

      (setq end (mk-vertex (mk-data "endwhile" (data-string (cl-graph:element $while)))))
      (mk-edge $while end)
      (setq c-end end)

      (mapcar #'build
	      (rest (rest expr)))

      (setq body recent-added)
      (setq recent-added $while)

      (if (equal (data-string (cl-graph:element body))
		 "endif")
	  (progn
	    (cl-graph:iterate-parents body (lambda (x)
					     (mk-edge x $while)))
	    (mk-edge body $while))
	  (mk-edge body $while))
      (mk-edge body $while)
      (setq c-while nil)
      (setq c-end nil)
      (setq recent-added end))))
