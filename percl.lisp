
(defpackage percl (:use :cl :cl-user :iter)
   (:import-from #:mongo-cl-driver.son-sugar son)
   (:export :identifable :id
			:database-base :db
			:generate-methods
			  :load-inst
			  :load-all-instances
			  :new-inst
			  :store-inst))

(in-package percl)

(defclass identifable () ((id :initform 0 :reader id :initarg :id))
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

(defun get-uniq-number (storage)
  (let ((counter (mongo:find-one (slot-value storage 'counters) (son "name" "uniq"))))
	(let ((val (gethash "c" counter)))
	  (incf (gethash "c" counter))
	  (mongo:update-op (slot-value storage 'counters) (son "name" "uniq") counter)
	  (floor val))))

(defun ensure-type (val type)
  (if type
	(let ((val-name (gensym)))
	  `(let ((,val-name ,val))
		 ,(ccase type
				 (integer
				   `(if (and (stringp ,val-name)
							 (string= "" ,val-name))
					  nil ,val-name)))))
	val))


(defun hash-getter (hash key) `(gethash ,key ,hash))
(defun prop-getter (list key) `(getf ,list ,key))

(defun mapargs (fun list)
  (mapcar #'(lambda (unit) (apply fun unit)) list))

(defun handle-set (getter set &key (bk nil))
  (flet ((option (select subst) (if bk 
								  `((equal ,subst ,getter) ,select)
								  `((equal ,select ,getter) ,subst))))
	(if set
	  `(cond 
		 ,@(mapargs #'option set)
		 (t ,getter))
	  getter)))

(defun doc-2-inst (doc getter class specs)
  (flet ((expand (slot name &key set type &allow-other-keys)
			(let ((val (ensure-type (funcall #'getter doc name) type)))
			  `(setf (slot-value inst ,slot)
					 ,(handle-set val set :bk t)))))
	`(let ((inst (make-instance ',class)))
	   ,@(mapargs #'expand (cons '('id "id") specs))
	   inst)))

(defun inst-2-doc (inst getter specs)
  (flet ((expand (slot name &key set &allow-other-keys)
		    (let ((val `(slot-value ,inst ,slot)))
			  `(setf ,(funcall #'getter 'doc name) ;(gethash ,name doc)
					 ,(handle-set val set)))))
	`(let ((doc (make-hash-table :test 'equal)))
	   ,@(mapargs #'expand (cons '('id "id") specs))
	   doc)))

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
						  ,(inst-2-doc 'inst #'hash-getter specs))))))

