
(in-package :percl)

(defclass mem-db ()
  ((next-num :accessor next-num :initform 1)))

(defmethod get-uniq-number ((db mem-db))
  (incf (next-num db)))

(defmethod retreive-one (id (db mem-db) (coll symbol))
  (gethash id (slot-value db coll)))

(defmethod retreive-all ((db mem-db) (coll symbol))
  (iter (for (id inst) in-hashtable (slot-value db coll))
		(collect inst)))

(defmethod write-value ((doc hash-table) (db mem-db) (coll symbol) new)
  (declare (ignore new))
  (setf (gethash (gethash "id" doc) (slot-value db coll)) doc))

