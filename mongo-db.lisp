
(in-package :percl)

(defclass mongo-db ()
  ((db :type mongo:database)
   (counters ))
  (:documentation "Basic class for data base, inheritors
				  must initialize the db slot by opening
				  a connection to a db server"))

(defmethod initialize-instance :after ((db mongo-db) &key)
  (setf (slot-value db 'counters) (mongo:collection (slot-value db 'db) "counters")))

(defmethod get-uniq-number ((db mongo-db))
  (let ((counter (mongo:find-one (slot-value db 'counters)
								 (son "name" "uniq"))))
	(let ((val (gethash "c" counter)))
	  (incf (gethash "c" counter))
	  (mongo:update-op (slot-value db 'counters)
					   (son "name" "uniq") counter)
	  (floor val))))

(defmethod retreive-one (id (db mongo-db) (coll symbol))
  (mongo:find-one (slot-value db coll) (son "id" id)))

(defmethod retreive-all ((db mongo-db) (coll symbol))
  (mongo:find-list (slot-value db coll) :query (son)))

(defmethod write-value ((doc hash-table) (db mongo-db) (coll symbol) new)
  (if new
	(mongo:insert-op (slot-value db coll) doc)
	(mongo:update-op (slot-value db coll) (son "id" (gethash "id" doc))
					 doc)))

