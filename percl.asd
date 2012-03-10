
(asdf:defsystem percl
				:description "Simple persistent manager"
				:depends-on (:mongo-cl-driver)
				:components ((:file "package")
							 (:file "general" :depends-on ("package"))
							 (:file "percl-id" :depends-on ("general"))
							 (:file "mongo-db" :depends-on ("general" "percl-id"))
							 (:file "percl" :depends-on ("general" "mongo-db"))))

