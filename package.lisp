

(defpackage percl (:use :cl :cl-user :iter)
   (:import-from #:mongo-cl-driver.son-sugar son)
   (:export :identifable :id
			:db-base :db
			:generate-methods
			  :generate-class-methods
			    :init-from-alist
			    :get-fields
			  :generate-db-methods
			    :load-inst
			    :load-all-instances
			    :store-inst))

(in-package :percl)
			  
(defgeneric load-inst (class id storage)
  (:documentation "Pull an instance of class gy id from storage"))
(defgeneric load-all-instances (class storage)
  (:documentation "Load whole collection of given class instances from
				  the storage"))
(defgeneric store-inst (inst storage)
  (:documentation "Save all changes of the given instance to the database, 
				  or insert it, if new "))
(defgeneric get-fields (class)
  (:documentation "Get the prop-list of all persistent fields of given class"))
(defgeneric init-from-alist (class alist)
  (:documentation "Create instance according to given association list, such
				  as the one, returned by hunchentoot:get-parameters*" ))

