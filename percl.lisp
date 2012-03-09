
(in-package percl)

(defmacro generate-methods (class (coll db) specs)
  `(progn
	 (generate-class-methods ,class ,specs)
	 (generate-db-methods ,class ,coll ,db)))

