
(asdf:operate 'asdf:load-op :percl)
(asdf:operate 'asdf:load-op :lift)

(defpackage percl-test
  (:use :cl :cl-user))

(in-package :percl-test)

(use-package :percl)
(use-package :lift)

(deftestsuite percl-test () ()
  (:equality-test #'equalp))

(defclass test-cl (identifable)
   ((sl1 :initarg :sl1)
    (sl2 :initarg :sl2)
	(sl-l :initarg :sl-l)))

(defclass test-cl-der (test-cl)
  ((sl3 :initarg :sl3)))

(defclass tst-db (mongo-db)
  (cont))

(defclass tst-mem-db (mem-db)
  (table))

(generate-methods test-cl ('cont tst-db)
				  (('sl1 "sl1" :set (('ans "42") ('res 45)))
				   ('sl2 "sl2")
				   ('sl-l "sl-l" :type list)))
(generate-methods test-cl-der ('cont tst-db) ( ('sl3 "sl3")))

(generate-methods test-cl ('table tst-mem-db)
				  (('sl1 "sl1" :set (('ans "42") ('res 45)))
				   ('sl2 "sl2") ;Redefenition test
				   ('sl-l "sl-l" :type list)))
(generate-methods test-cl-der ('table tst-mem-db) ( ('sl3 "sl3")))

(addtest (percl-test) id-present
  (ensure-same (get-fields 'identifable)
			   '(:id "id")))

(addtest (percl-test) fresh-inst
  (ensure-same (slot-value (make-instance 'test-cl) 'id) 0))

(addtest (percl-test) basic-init
  (ensure-same (slot-value (init-from-alist 'identifable '(("id" . "18"))) 'id)
			   18))

(addtest (percl-test) inheritance
  (ensure-same (get-fields 'test-cl-der)
			   '(:id "id" :sl1 "sl1" :sl2 "sl2" :sl-l "sl-l" :sl3 "sl3")))

(addtest (percl-test) inherited-init
  (let ((inst (init-from-alist 'test-cl-der '(("sl2" . 33) ("id" . 3) ("sl3" . 8500)))))
	(ensure-same (slot-value inst 'id) 3)
	(ensure-same (slot-value inst 'sl2) 33)
	(ensure-same (slot-value inst 'sl3) 8500)))

(addtest (percl-test) set-parse-test
  (ensure-same
	(slot-value (init-from-alist 'test-cl '(("sl1" . 45))) 'sl1) 'res)
  (ensure-same
	(slot-value (init-from-alist 'test-cl '(("sl1" . "42"))) 'sl1) 'ans)
  (ensure-same
	(slot-value (init-from-alist 'test-cl '(("sl1" . 42))) 'sl1) 42))

(addtest (percl-test) list-parse-test
  (ensure-same
	(slot-value (init-from-alist 'test-cl
								 '(("sl-l" . " (3 4 2 \"o-lala\" )"))) 'sl-l)
	'(3 4 2 "o-lala")))


(defmethod initialize-instance ((db tst-mem-db) &key)
  (setf (slot-value db 'table) (make-hash-table :test 'equalp)))

(defmethod initialize-instance ((db tst-db) &key)
  (setf (slot-value db 'db) (make-instance 'mongo:database :name "test"))
  (setf (slot-value db 'cont) (mongo:collection (slot-value db 'db) "tt")))

(deftestsuite percl-db-test (percl-test)
  ((*mongo-db* (make-instance 'tst-db));'tst-mem-db));'tst-db))
   (id)) )

(addtest (percl-db-test) db-store
  (let ((inst (make-instance 'test-cl :sl1 3 :sl2 "str" :sl-l '(5 3 1))))
	(store-inst inst *mongo-db*)
	(setf id (id inst))
	(ensure-different id 0)))

(addtest (percl-db-test) db-load
  (let ((inst (load-inst 'test-cl *mongo-db* :id id)))
	(ensure-same (id inst) id)
	(ensure-same (slot-value inst 'sl1) 3)
	(ensure-same (slot-value inst 'sl2) "str")
	(ensure-same (slot-value inst 'sl-l) '(5 3 1))))

(addtest (percl-db-test) query-one
  (let ((inst (load-inst 'test-cl *mongo-db* :query '(sl1 3))))
	(ensure inst)
	(ensure-same (slot-value inst 'sl1) 3)))

(addtest (percl-db-test) db-change-inst
  (let ((inst (load-inst 'test-cl *mongo-db* :id id)))
	(setf (slot-value inst 'sl1) 8)
	(store-inst inst *mongo-db*)
	(ensure-same id (id inst))
	(ensure-same (slot-value (load-inst 'test-cl *mongo-db* :id id) 'sl1) 8)))

(addtest (percl-db-test) db-load-all
  (let ((all (load-all-instances 'test-cl *mongo-db*)))
	(ensure-same (type-of all) 'cons)))

(addtest (percl-db-test) db-remove-one
  (remove-inst id 'test-cl *mongo-db*)
  (ensure-null (load-inst 'test-cl *mongo-db* :id id)))

(let* ((rez (run-tests :suite 'percl-test))
	   (errs (lift:errors rez))
	   (fails (lift:failures rez)))
  (print rez)
  (if (or errs fails)
	(describe rez)
	(sb-ext:quit)))

