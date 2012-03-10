
(in-package :percl)

(defclass db-base ()
  ((db :type mongo:database)
   (counters ))
  (:documentation "Basic class for data base, inheritors
				  must initialize the db slot by opening
				  a connection to a db server"))

(defmethod initialize-instance :after ((db db-base) &key)
  (setf (slot-value db 'counters) (mongo:collection (slot-value db 'db) "counters")))

(defun get-uniq-number (storage)
  (let ((counter (mongo:find-one (slot-value storage 'counters) (son "name" "uniq"))))
	(let ((val (gethash "c" counter)))
	  (incf (gethash "c" counter))
	  (mongo:update-op (slot-value storage 'counters) (son "name" "uniq") counter)
	  (floor val))))


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
							,(make-doc-from-inst 'inst)))
		 (mongo:update-op (slot-value db ,coll) (son "id" (id inst)) 
						  ,(make-doc-from-inst 'inst))))))
