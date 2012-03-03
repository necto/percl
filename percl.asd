
(asdf:defsystem percl
				:description "Simple persistent manager"
				:depends-on (:mongo-cl-driver)
				:components ((:file "percl")))

