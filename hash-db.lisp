
(in-package :percl)

(defgeneric get-uniq-number (db))
(defgeneric retreive-one (id db coll))
(defgeneric retreive-all (db coll))
(defgeneric write-value (doc db coll new))

(defun make-inst-from-doc (doc class)
  `(let ((inst (make-instance ',class)))
	 (doc>inst inst ,doc)
	 inst))

(defun make-doc-from-inst (inst)
  `(let ((doc (make-hash-table :test 'equal)))
	 (inst>doc ,inst doc)
	 doc))

(defmacro generate-db-methods (class coll db)
  `(progn
	 (defmethod load-inst ((class (eql ',class)) id (db ,db))
	   (let ((doc (retreive-one id db ,coll)))
		 (when doc ,(make-inst-from-doc 'doc class))))
	 (defmethod load-all-instances ((class (eql ',class)) (db ,db))
	   (iter (for doc in (retreive-all db ,coll))
			 (collect ,(make-inst-from-doc 'doc class))))

	 (defmethod store-inst ((inst ,class) (db ,db))
	   (if (= 0 (id inst)) ;unaccounted instance
		 (progn
		   (setf (slot-value inst 'id) (get-uniq-number db))
		   (write-value ,(make-doc-from-inst 'inst) db ,coll t))
	     (write-value ,(make-doc-from-inst 'inst) db ,coll nil)))))

