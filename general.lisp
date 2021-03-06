
(in-package :percl)

(defgeneric doc>inst (inst doc) (:method-combination progn)
  (:documentation "Get data from hash-table doc to an instance inst"))
(defgeneric inst>doc (inst doc) (:method-combination progn)
  (:documentation "Put data from instance into hash-table doc"))
(defgeneric append-fields (class extra)
  (:documentation "Push a list of inherited fields extra, to the class list"))
(defgeneric reset-fields (class)
  (:documentation "Leave only class's own fields in the list."))
(defgeneric direct-fields (class)
  (:documentation "Get the list of fields whicha are owned by this class,
				   not predcessors"))
(defgeneric alist>inst (inst alist) (:method-combination progn)
  (:documentation "Initialize instance inst from associative list alist"))

(defun parse-list (str)
  "convert a string in format \" ( a e 33  5) \" to a list (a e 33 5)"
  (when (and str (not (string= "" str)))
	(with-input-from-string (is str)
	  (read is))))

(defun ensure-type (val type)
  "Make necessary transformations of val in order
   to convert it to given type"
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
  "Convert the value (obtained by getter) to a one of elements of the set
   like C-enums"
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
  "Fill slots of the inst by the appropriate entries of the doc"
  (flet ((expand (slot name &key set type &allow-other-keys)
			(let ((val (ensure-type (funcall getter doc name) type)))
			  `(setf (slot-value ,inst ,slot)
					 ,(handle-set val set :bk t)))))
	(mapargs #'expand specs)))

(defun pump-from-inst (inst getter doc specs)
  "Get slot values from inst, and write them to doc"
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
  "Convert list-of lists to a property list with keys from KEYWORD package"
  (flatten 
	(mapargs 
	  #'(lambda (slot name &key &allow-other-keys)
		  `(,(intern (symbol-name (second slot)) "KEYWORD")
			 ,name))
	  specs)))

(defun pump-fields (cl1 cl2)
  "Look at these two classes, and add all fields of a predcessor
   to a successor, by appending a list of them"
  (when (subtypep cl1 cl2)
	(append-fields cl1 (direct-fields cl2)))
  (when (subtypep cl2 cl1)
	(append-fields cl2 (direct-fields cl1))))

(defun reset-inheritance (got-classes)
  (iter (for cl in got-classes)
		(reset-fields cl)))

(defun establish-inheritance (got-classes)
  (iter (for subset on got-classes)
		(iter (for cl in (cdr subset))
			  (pump-fields cl (car subset)))))

(defun reestablish-inheritance (got-classes)
  "Refresh all inherited fields"
  (reset-inheritance got-classes)
  (establish-inheritance got-classes))

(let (got-classes)
  (defun handle-fields (cl)
	"Implement inheritance in a humble form using subtypep predicate"
	(if (find cl got-classes)
	  (reestablish-inheritance got-classes) ;redefenition, reload.
	  (progn
		(iter (for other in got-classes) ;all this stuff needed just
			  (pump-fields cl other))    ;in order to replace MOP
		(pushnew cl got-classes)))))

(defmacro generate-class-methods (class specs)
  "Generate all neccessery methods to work with database
   specs must be a list of the important fields of the class
   which you want to be saved, and loaded in the certain format."
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
	   (defmethod get-field-name ((class (eql ',class)) (slot symbol))
		 (getf fields (intern (symbol-name slot) "KEYWORD")))
	   (defmethod direct-fields ((class (eql ',class))) direct-fields)
	   (defmethod append-fields ((class (eql ',class)) extra)
		 (setf fields (append extra fields)))
	   (defmethod reset-fields ((class (eql ',class)))
		 (setf fields direct-fields))
	   (handle-fields ',class))

	 (defmethod init-from-alist ((class (eql ',class)) alist)
	   (let ((inst (make-instance class)))
		 (alist>inst inst alist)
		 inst))))

