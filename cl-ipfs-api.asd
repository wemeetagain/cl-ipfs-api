(cl:in-package #:asdf-user)

(defsystem :cl-ipfs-api
  :version "0.0.1"
  :description "A client library for the IPFS API"
  :author "Cayman Nava"
  :license "MIT"
  :defsystem-depends-on (protobuf)
  :depends-on (:alexandria :cl-multiaddr :dexador :flexi-streams :jonathan :protobuf :quri :yason)
  :components (
	       ;(:module "pb"
		;:components
		;((:protobuf-source-file "dht")
		; (:protobuf-source-file "merkeldag")
		; (:protobuf-source-file "unixfs")))
	       (:module "src"
		;:depends-on ("pb")
		:components
		((:file "package")
		 (:file "config")
		 (:file "util")
		 (:file "encoding")
		 (:static-file "definitions.json")
		 (:file "request-api" :depends-on ("config" "util" "encoding"))
		 (:file "command" :depends-on ("request-api" "definitions.json")))))
  :long-description #.(uiop:read-file-string
		       (uiop:subpathname *load-pathname* "README.md"))
  :in-order-to ((test-op (test-op cl-ipfs-api-test))))
		 
