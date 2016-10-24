(cl:in-package #:asdf-user)

(defsystem :cl-ipfs-api-test
  :description "cl-ipfs-api unit tests"
  :license "MIT"
  :depends-on (:babel :cl-ipfs-api :jonathan :prove :prove-asdf)
  :defsystem-depends-on (:prove-asdf)
  :components ((:module "t"
                :components
                ((:test-file "cl-ipfs-api"))))
  :perform (test-op :after (op c)
                    (funcall (intern #.(string :run) :prove) c)))
