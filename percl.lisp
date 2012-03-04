
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

(defclass identifable () ((id :initform 0 :reader id :type integer))
  (:documentation "A basic class for all instances to be stored in database"))

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
(defgeneric new-inst (class storage)
  (:documentation "Create a new instance of the given class,
				  having uniq (for given database) id, and insert it there"))
(defgeneric store-inst (inst storage)
  (:documentation "Save all changes of the given instance to the database, 
				  or insert it, if new "))
(defgeneric get-fields (class)
  (:documentation "Get the prop-list of all persistent fields of given class"))
(defgeneric init-from-alist (class alist)
  (:documentation "Create instance according to given association list, such
				  as the one, returned by hunchentoot:get-parameters*" ))

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

(defun doc-2-inst (doc getter class specs)
  (flet ((expand (slot name &key set type &allow-other-keys)
			(let ((val (ensure-type (funcall getter doc name) type)))
			  `(setf (slot-value inst ,slot)
					 ,(handle-set val set :bk t)))))
	`(let ((inst (make-instance ',class)))
	   ,@(mapargs #'expand (cons '('id "id" :type integer) specs))
	   inst)))

(defun inst-2-doc (inst getter specs)
  (flet ((expand (slot name &key set &allow-other-keys)
		    (let ((val `(slot-value ,inst ,slot)))
			  `(setf ,(funcall getter 'doc name) ;(gethash ,name doc)
					 ,(handle-set val set)))))
	`(let ((doc (make-hash-table :test 'equal)))
	   ,@(mapargs #'expand (cons '('id "id" :type integer) specs))
	   doc)))

(defun flatten (tree &optional tail)
  (if (atom tree) (cons tree tail)
	(flatten (car tree) (if (cdr tree)
						  (flatten (cdr tree) tail)
						  tail))))

(defmacro generate-methods (class (coll db) specs)
  `(progn
	 (defmethod load-inst ((class (eql ',class)) id (db ,db))
	   (let ((doc (mongo:find-one (slot-value db ,coll) (son "id" id))))
		 (when doc
		   ,(doc-2-inst 'doc #'hash-getter class specs))))
	 (defmethod load-all-instances ((class (eql ',class)) (db ,db))
	   (iter (for doc in (mongo:find-list (slot-value db ,coll) :query (son)))
			 (collect ,(doc-2-inst 'doc #'hash-getter class specs))))

	 (defmethod new-inst ((class (eql ',class)) (db ,db))
	   (let ((id (get-uniq-number db)))
		 (let ((doc (make-hash-table :test 'equal)))
		   (setf (gethash "id" doc) id)
		   (mongo:insert-op (slot-value db ,coll) doc))
		 (load-inst ',class id db)))

	 (defmethod store-inst ((inst ,class) (db ,db))
	   (if (= 0 (id inst)) ;unaccounted instance
		 (progn
		   (setf (slot-value inst 'id) (get-uniq-number db))
		   (mongo:insert-op (slot-value db ,coll)
							,(inst-2-doc 'inst #'hash-getter specs)))
		 (mongo:update-op (slot-value db ,coll) (son "id" (id inst)) 
						  ,(inst-2-doc 'inst #'hash-getter specs))))

	 (defmethod get-fields ((class (eql ',class)))
	   ',(flatten 
		   (append (mapargs #'(lambda (slot name &key &allow-other-keys)
								`(,(intern (symbol-name (second slot)) "KEYWORD") ,name))
							specs)
				   '(:id "id"))))
	 (defmethod init-from-alist ((class (eql ',class)) alist)
	   ,(doc-2-inst 'alist #'alist-getter class specs))))

