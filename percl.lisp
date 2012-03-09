
(defpackage percl (:use :cl :cl-user :iter)
   (:import-from #:mongo-cl-driver.son-sugar son)
   (:export :identifable :id
			:database-base :db
			:generate-methods
			  :load-inst
			  :load-all-instances
			  :new-inst
			  :store-inst
			  :get-fields
			  :init-from-alist))

(in-package percl)

(defclass database-base ()
  ((db :type mongo:database)
   (counters ))
  (:documentation "Basic class for data base, inheritors
				  must initialize the db slot by opening
				  a connection to a db server"))

(defmethod initialize-instance :after ((db database-base) &key)
  (setf (slot-value db 'counters) (mongo:collection (slot-value db 'db) "counters")))

(defgeneric load-inst (class id storage)
  (:documentation "Pull an instance of class gy id from storage"))
(defgeneric load-all-instances (class storage)
  (:documentation "Load whole collection of given class instances from
				  the storage"))
(defgeneric store-inst (inst storage)
  (:documentation "Save all changes of the given instance to the database, 
				  or insert it, if new "))
(defgeneric get-fields (class)
  (:documentation "Get the prop-list of all persistent fields of given class"))
(defgeneric init-from-alist (class alist)
  (:documentation "Create instance according to given association list, such
				  as the one, returned by hunchentoot:get-parameters*" ))

(defgeneric doc>inst (inst doc) (:method-combination progn))
(defgeneric inst>doc (inst doc) (:method-combination progn))
(defgeneric append-fields (class extra))
(defgeneric direct-fields (class))
(defgeneric alist>inst (inst alist) (:method-combination progn))

(defvar *got-classes* ())

(defun get-uniq-number (storage)
  (let ((counter (mongo:find-one (slot-value storage 'counters) (son "name" "uniq"))))
	(let ((val (gethash "c" counter)))
	  (incf (gethash "c" counter))
	  (mongo:update-op (slot-value storage 'counters) (son "name" "uniq") counter)
	  (floor val))))

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

(defun pump-doc-to-inst (doc getter inst specs)
  (flet ((expand (slot name &key set type &allow-other-keys)
			(let ((val (ensure-type (funcall getter doc name) type)))
			  `(setf (slot-value ,inst ,slot)
					 ,(handle-set val set :bk t)))))
	(mapargs #'expand specs)))

(defun pump-inst-to-doc (inst getter doc specs)
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

(defun make-inst-from-doc (doc class)
  `(let ((inst (make-instance ',class)))
	 (doc>inst inst ,doc)
	 inst))

(defun specs>prop-list (specs)
  (flatten 
	(mapargs 
	  #'(lambda (slot name &key &allow-other-keys)
		  `(,(intern (symbol-name (second slot)) "KEYWORD")
			 ,name))
	  specs)))

;(defmacro generate-class-methods (class specs)
;  `(progn

;(defmacro generate-db-methods (class (coll db))
;  `(progn

(defun sm-fn (cl)
  (format t "~% ()()()()()(()())()())()()()()()()()()()()()()() == ~a~%" cl))


(defmacro generate-methods (class (coll db) specs)
  (pushnew class *got-classes*)
  (print *got-classes*)
  `(progn
	 (defmethod doc>inst progn ((inst ,class) (doc hash-table))
	   ,@(pump-doc-to-inst 'doc #'hash-getter 'inst specs))

	 (defmethod alist>inst progn ((inst ,class) (alist list))
	   (format t "getted from alist ~a -> ~a ~%" alist ',class)
	   ,@(pump-doc-to-inst 'alist #'alist-getter 'inst specs))

	 (defmethod inst>doc progn ((inst ,class) (doc hash-table))
	   ,@(pump-inst-to-doc 'inst #'hash-getter 'doc specs))

	 (let* ((direct-fields ',(specs>prop-list specs))
			(fields direct-fields))
	   (defmethod get-fields ((class (eql ',class)))  fields)
	   (defmethod direct-fields ((class (eql ',class))) direct-fields)
	   (defmethod append-fields ((class (eql ',class)) extra)
		 (format t "called for ~a with ~a~%" class extra)
		 (setf fields (append extra fields)))
	   ,@(mapcar #'(lambda (succ)
					 (when (not (eq succ class))    ;all this stuff needed just
					   (when (subtypep succ class)  ;in order to replace MOP
						 `(append-fields ',succ fields))
					   (when (subtypep class succ)
						 `(append-fields ',class (direct-fields ',succ)))))
				 *got-classes*)
	   (append-fields ',class (direct-fields ',class))
	   (sm-fn ',class) ; Why isn't it called in server.lisp, but is called in percl-test.lisp ???
	   )
	 
	 (defmethod init-from-alist ((class (eql ',class)) alist)
	   (let ((inst (make-instance class)))
		 (alist>inst inst alist)
		 inst))

	 (defmethod load-inst ((class (eql ',class)) id (db ,db))
	   (let ((doc (mongo:find-one (slot-value db ,coll) (son "id" id))))
		 (when doc ,(make-inst-from-doc 'doc class))))

	 (defmethod load-all-instances ((class (eql ',class)) (db ,db))
	   (iter (for doc in (mongo:find-list (slot-value db ,coll) :query (son)))
			 (collect ,(make-inst-from-doc 'doc class))))

	 (defmethod store-inst ((inst ,class) (db ,db))
	   (if (= 0 (id inst)) ;unaccounted instance
		 (progn
		   (setf (slot-value inst 'id) (get-uniq-number db))
		   (mongo:insert-op (slot-value db ,coll)
							(let ((doc (make-hash-table :test 'equal)))
							  (inst>doc inst doc)
							  doc)))
		 (mongo:update-op (slot-value db ,coll) (son "id" (id inst)) 
						  (let ((doc (make-hash-table :test 'equal)))
							(inst>doc inst doc)
							doc))))))



