
(in-package :percl)

(defclass mem-db ()
  ((next-num :accessor next-num :initform 1)))

(defmethod initialize-instance :before ((db mem-db) &key)
  (setf (slot-value db 'next-num) 1))

(defmethod get-uniq-number ((db mem-db))
  (incf (next-num db)))

(defmethod adopt-query (query (db mem-db) class)
  (let ((name (get-field-name class (first query)))
		(val (second query)))
    (lambda (doc)
	  (eq (gethash name doc) val))))

(defmethod retreive-one ((db mem-db) (coll symbol) id query)
  (if id
    (gethash id (slot-value db coll))
	(if query
	  (iter (for (id inst) in-hashtable (slot-value db coll))
			(finding inst such-that (funcall query inst))))))

(defmethod retreive-all ((db mem-db) (coll symbol) query)
  (if query
	(iter (for (id inst) in-hashtable (slot-value db coll))
		  (if (funcall query inst) (collect inst)))
    (iter (for (id inst) in-hashtable (slot-value db coll))
		  (collect inst))))

(defmethod write-value ((doc hash-table) (db mem-db) (coll symbol) new)
  (declare (ignore new))
  (setf (gethash (gethash "id" doc) (slot-value db coll)) doc))

