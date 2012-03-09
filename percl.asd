
(asdf:defsystem percl
				:description "Simple persistent manager"
				:depends-on (:mongo-cl-driver)
				:components ((:file "package")
							 (:file "general" :depends-on ("package"))
							 (:file "mongo-db" :depends-on ("general" "package"))
							 (:file "percl" :depends-on ("general" "mongo-db"))
							 (:file "percl-id" :depends-on ("percl"))))

