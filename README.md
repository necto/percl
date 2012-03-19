 A simple persistence manager for common lisp. Currently it supports only mongo.db via mongo-db-driver.

 The following features are now supported:
  * Multiple databases
  * Memory database, does not need any additional server
  * Inheritance of registred classes (via type-of predicate)
  * Redefinition of classes, with update all inheritance information
  * Reduction values to cpecified types.
  * Sets for values (analogue of C enums) - automatic cast.
  * Abstract interface to database driver for easy addition new ones (currently tested only on MongoDB and mem-db:)

 For usage you just need (look at percl-test.lisp for advanced example):

```cl
    (asdf:operate 'asdf:load-op :percl)
    (use-package :percl)


; Define some class, and database:

    (defclass Myth (identifable)
      ((story :initarg :story :accessor my-story)
       (name :initarg :name :accessor my-name)))
    (defclass DB (mongo-db) (myths))

    
    (defmethod initialize-instance ((db DB) &key)
      (setf (slot-value db 'db) (make-instance 'mongo:database :name "test"))
      (setf (slot-value db 'myths) (mongo:collection (slot-value db 'db) "myths")))
    
    (defparameter *db* (make-instance 'DB))

; And generate appropriate methods
; (see (documentation for generic functions in percl package)):
    
    (generate-methods Myth ('myths DB) (('name "name") ('story "story")))

; And then enjoy storing, and loading instances to/from database test:

    (defvar id nil) ;for not to lose an instance.

    (let ((i (make-instance 'Myth :name "Uranus"
                :story
                 "Uranus is the sky god and first ruler. He is the
                 son of Gaea, who created him without help. He then
                 became the husband of Gaea and together they had many
                 offspring, including twelve of the Titans. ")))
      ; Note, that here (id i) is false yet,
      ; because it didn't interact with database.
      (store-inst i *db*)
      (setf id (id i))) ;But here it is real

; Then in your database appear a new entry:
;   > db.myths.find()
;   { "_id" : ObjectId("4f578a34dc96e0c99972b8a8"), "id" : 134, "name" : "Uranus", "story" : "Uranus is the sky god and first ruler. He is the\n                 son of Gaea, who created him without help. He then\n                 became the husband of Gaea and together they had many\n                 offspring, including twelve of the Titans. " }
 
; After it you may want to work with this object, and save results of your work:
    
    (let ((i (load-inst 'Myth *db* :id id)))
      (print (my-name i))                  ;-> "Uranus"
      (setf (my-story i) "Uranus have the long byography...")
      (store-inst i *db*))

; Then in your database appear a new entry:
;   > db.myths.find()
;   { "_id" : ObjectId("4f578a34dc96e0c99972b8a8"), "id" : 134, "name" : "Uranus", "story" : "Uranus have the long byography..." }

```
