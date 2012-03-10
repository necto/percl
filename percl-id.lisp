(in-package percl)

(defclass identifable () ((id :initform 0 :reader id :type integer))
  (:documentation "A base class for all instances to be handled
				   by the store-inst and loade-inst functions."))

(generate-class-methods identifable (('id "id" :type integer)))

