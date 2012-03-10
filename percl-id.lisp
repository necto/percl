(in-package percl)

(defclass identifable () ((id :initform 0 :reader id :type integer))
  (:documentation "A basic class for all instances to be stored in database"))

;(print (macroexpand-1 '(generate-methods identifable ('counters database-base) (('id "id" :type integer)))))
;(print (macroexpand-1 '(generate-class-methods identifable  (('id "id" :type integer)))))

(generate-class-methods identifable (('id "id" :type integer)))

