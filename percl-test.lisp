
(asdf:operate 'asdf:load-op :percl)

(use-package :percl)

(defclass test (identifable)
   ((sl1 :initarg :sl1)
   (sl2 :initarg :sl2)))

(defclass test-der (test)
  ((sl3 :initarg :sl3)))

(defclass tst-db (database-base)
  (cont))

(defmethod initialize-instance ((db tst-db) &key) ;TODO: know more about method sequence
  (setf (slot-value db 'db) (make-instance 'mongo:database :name "test"))
  (setf (slot-value db 'cont) (mongo:collection (slot-value db 'db) "tt")))

(defvar *db* (make-instance 'tst-db))

(print (macroexpand-1 '(generate-methods test ('cont tst-db) (('sl1 "sl1" :set (('ans 42) ('res 33))) ('sl2 "sl2")))))
(generate-methods test ('cont tst-db) (('sl1 "sl1" :set (('ans "42") ('res 45))) ('sl2 "sl2")))
(print (macroexpand-1 '(generate-methods test-der ('cont tst-db) ( ('sl3 "sl3")))))
(generate-methods test-der ('cont tst-db) ( ('sl3 "sl3")))


(let ((ii (load-inst 'test 119 *db*)) )
  (print (load-all-instances 'test *db*))
	(print (slot-value ii 'sl1))
	(print (slot-value ii 'id))
  (setf (slot-value ii 'sl1) 'res)
  (store-inst ii *db*)

  (print (slot-value (init-from-alist 'test '(("id" . 34) ("sl1" . 45) ("sl2" . 2)))
					 'id))
;	  (aa (make-instance 'test)))
;  (setf (slot-value aa 'sl1) 42)
;  (setf (slot-value aa 'sl2) 42)
;  (store-inst aa *db*)

	(print (slot-value ii 'sl1)) 
	
	(print (get-fields 'test-der))
	)
;	(print (load-all-instances 'test *db*))

(sb-ext:quit)

