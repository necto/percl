
(asdf:defsystem percl
				:description "Simple persistent manager"
				:depends-on (:mongo-cl-driver)
				:components ((:file "percl")
							 (:file "percl2" :depends-on ("percl"))))

