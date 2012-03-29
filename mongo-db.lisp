
(in-package :percl)

(defclass mongo-db ()
  ((db :type mongo:database)
   (counters ))
  (:documentation "Basic class for data base, inheritors
				  must initialize the db slot by opening
				  a connection to a db server"))

(defmethod initialize-instance :after ((db mongo-db) &key)
  (setf (slot-value db 'counters) (mongo:collection (slot-value db 'db) "counters")))

(defmethod adopt-query (query (db mongo-db) class)
  (son (get-field-name class (first query)) (second query)))

(defmethod get-uniq-number ((db mongo-db))
  (let ((counter (mongo:find-one (slot-value db 'counters)
								 (son "name" "uniq"))))
	(let ((val (gethash "c" counter)))
	  (incf (gethash "c" counter))
	  (mongo:update-op (slot-value db 'counters)
					   (son "name" "uniq") counter)
	  (floor val))))

(defmethod retreive-one ((db mongo-db) (coll symbol) id query)
  (mongo:find-one (slot-value db coll) (if id 
										 (son "id" id)
										 query)))

(defmethod retreive-all ((db mongo-db) (coll symbol) query)
  (mongo:find-list (slot-value db coll) :query (if query query (son))))

(defmethod write-value ((doc hash-table) (db mongo-db) (coll symbol) new)
  (if new
	(mongo:insert-op (slot-value db coll) doc)
	(mongo:update-op (slot-value db coll) (son "id" (gethash "id" doc))
					 doc)))

(defmethod remove-from-coll (id (coll symbol) (storage mongo-db))
  (mongo:delete-op (slot-value storage coll) (son "id" id)))

