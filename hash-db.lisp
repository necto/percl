
(in-package :percl)

(defgeneric get-uniq-number (db)
	(:documentation "Generate certanly uniq number, for identifiers"))
(defgeneric retreive-one (db coll id query)
	(:documentation "Must be supplyed only one: either id or query, not both."))
(defgeneric retreive-all (db coll query)
	(:documentation "null query means retreive each entry"))
(defgeneric write-value (doc db coll new)
	(:documentation "store given value (hash-table) to the given collection
					 in the database db. If the value created in application
					 the new will be t"))
(defgeneric adopt-query (query db class)
	(:documentation "transform query to the format understood by database"))

(defgeneric remove-from-coll (id coll db))

(defun make-inst-from-doc (doc class)
  `(let ((inst (make-instance ',class)))
	 (doc>inst inst ,doc)
	 inst))

(defun make-doc-from-inst (inst)
  `(let ((doc (make-hash-table :test 'equal)))
	 (inst>doc ,inst doc)
	 doc))

(defun handle-query (q db cl)
  `(if ,q 
	 (adopt-query ,q ,db ',cl)))

(defmacro generate-db-methods (class coll db)
  `(progn
	 (defmethod load-inst ((class (eql ',class)) (db ,db) &key id query)
	   (let ((doc (retreive-one db ,coll id ,(handle-query 'query 'db class))))
		 (when doc ,(make-inst-from-doc 'doc class))))
	 (defmethod load-all-instances ((class (eql ',class)) (db ,db) &key query)
	   (iter (for doc in (retreive-all db ,coll ,(handle-query 'query 'db class)))
			 (collect ,(make-inst-from-doc 'doc class))))
	 (defmethod remove-inst (id (class (eql ',class)) (db ,db))
	   (remove-from-coll id ,coll db))

	 (defmethod store-inst ((inst ,class) (db ,db))
	   (if (= 0 (id inst)) ;unaccounted instance
		 (progn
		   (setf (slot-value inst 'id) (get-uniq-number db))
		   (write-value ,(make-doc-from-inst 'inst) db ,coll t))
	     (write-value ,(make-doc-from-inst 'inst) db ,coll nil)))))

