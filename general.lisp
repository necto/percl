
(in-package :percl)

(defgeneric doc>inst (inst doc) (:method-combination progn))
(defgeneric inst>doc (inst doc) (:method-combination progn))
(defgeneric append-fields (class extra))
(defgeneric direct-fields (class))
(defgeneric alist>inst (inst alist) (:method-combination progn))

(defun parse-list (str)
  (when (and str (not (string= "" str)))
	(with-input-from-string (is str)
	  (read is))))

(defun ensure-type (val type)
  (if (not type)
	val
	(let ((val-name (gensym)))
	  `(let ((,val-name ,val))
		 ,(ccase type
				 (string val-name)
				 (integer
				   `(if (stringp ,val-name)
					   (if (string= "" ,val-name)
						 nil
						 (parse-integer ,val-name))
					   ,val-name))
				 (boolean
				   `(not (null ,val-name)))
				 (list
				   `(if (stringp ,val-name)
					  (if (string= "" ,val-name)
						nil
						(parse-list ,val-name))
					  ,val-name)))))))

(defun hash-getter (hash key) `(gethash ,key ,hash))
(defun alist-getter (list key) `(cdr (assoc ,key ,list :test #'equal)))

(defun mapargs (fun list)
  (mapcar #'(lambda (unit) (apply fun unit)) list))

(defun handle-set (getter set &key (bk nil))
  (let ((val-name (gensym)))
	(flet ((option (select subst)
			 (if bk 
				`((equal ,subst ,val-name) ,select)
				`((equal ,select ,val-name) ,subst))))
	  (if set
		`(let ((,val-name ,getter))
		   (cond 
			  ,@(mapargs #'option set)
			  (t ,val-name)))
		getter))))

(defun pump-to-inst (doc getter inst specs)
  (flet ((expand (slot name &key set type &allow-other-keys)
			(let ((val (ensure-type (funcall getter doc name) type)))
			  `(setf (slot-value ,inst ,slot)
					 ,(handle-set val set :bk t)))))
	(mapargs #'expand specs)))

(defun pump-from-inst (inst getter doc specs)
  (flet ((expand (slot name &key set &allow-other-keys)
		    (let ((val `(slot-value ,inst ,slot)))
			  `(setf ,(funcall getter doc name)
					 ,(handle-set val set)))))
	(mapargs #'expand specs)))

(defun flatten (tree &optional tail)
  (if (atom tree) (cons tree tail)
	(flatten (car tree) (if (cdr tree)
						  (flatten (cdr tree) tail)
						  tail))))

(defun specs>prop-list (specs)
  (flatten 
	(mapargs 
	  #'(lambda (slot name &key &allow-other-keys)
		  `(,(intern (symbol-name (second slot)) "KEYWORD")
			 ,name))
	  specs)))

(defun pump-fields (cl1 cl2)
  (when (subtypep cl1 cl2)
	(append-fields cl1 (direct-fields cl2)))
  (when (subtypep cl2 cl1)
	(append-fields cl2 (direct-fields cl1))))

(let (*got-classes*)
  (defun handle-fields (cl)
    (unless (find cl *got-classes*)
	  (iter (for other in *got-classes*) ;all this stuff needed just
		    (pump-fields cl other))      ;in order to replace MOP
	  (pushnew cl *got-classes*))))

(defmacro generate-class-methods (class specs)
  `(progn
	 (defmethod doc>inst progn ((inst ,class) (doc hash-table))
	   ,@(pump-to-inst 'doc #'hash-getter 'inst specs))

	 (defmethod alist>inst progn ((inst ,class) (alist list))
	   ,@(pump-to-inst 'alist #'alist-getter 'inst specs))

	 (defmethod inst>doc progn ((inst ,class) (doc hash-table))
	   ,@(pump-from-inst 'inst #'hash-getter 'doc specs))

	 (let* ((direct-fields ',(specs>prop-list specs))
			(fields direct-fields))
	   (defmethod get-fields ((class (eql ',class)))  fields)
	   (defmethod direct-fields ((class (eql ',class))) direct-fields)
	   (defmethod append-fields ((class (eql ',class)) extra)
		 (setf fields (append extra fields)))
	   (handle-fields ',class))))

