(cl:in-package #:asdf-user)

(defsystem :cl-ipfs-api
  :version "0.0.1"
  :description "A client library for the IPFS API"
  :author "Cayman Nava"
  :license "MIT"
  :depends-on (:alexandria :dexador :jonathan :quri :yason)
  :components ((:module "src"
		:components
		((:file "package")
		 (:file "config")
		 (:file "with-ipfs-connection")
		 (:file "util")
		 (:file "encoding")
		 (:static-file "definitions.json")
		 (:file "request-api" :depends-on ("config" "util" "encoding"))
		 (:file "command" :depends-on ("request-api" "definitions.json")))))
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-ipfs-api-test))))
		 
