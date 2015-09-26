(cl:in-package #:cl-user)

(defpackage #:cl-ipfs-api-test
  (:use #:cl #:prove))
(in-package #:cl-ipfs-api-test)

(defun ipfs-command (name args opts)
  (let ((output (uiop:run-program (concatenate
				   'string
				   "ipfs "
				   (format nil "~{~A ~}" name)
				   (format nil "~:{--~A ~A ~}" opts)
				   (format nil "~{~A ~}" (mapcar (lambda (x)
								   (if (null x)
								       ""
								       x))
								 args)))
				  :output :string)))
    (if (string= (or (cadr (assoc "encoding" opts :test #'string=))
		     "")
		 "json")
	(jonathan:parse output :as :alist)
	output)))

(defvar *test-dir* "testdir/")
(defvar *test-file* "file1.txt")
(defvar *test-file-data* "hello test")
(defvar *test-dir-path* (pathname *test-dir*))
(defvar *test-file-path* (pathname (concatenate 'string
						*test-dir*
						*test-file*)))
(defvar *test-file-hash* "QmRv6FrRUqB8WkSn5FGa4QrsmBvyKTPrGamF43vYEAmPo4")
(defvar *test-dir-hash* "QmNXT2U3VybnKDwwBSKWDwsLdyA1EQLYa6CVnPSTDSb9XA")

(defun make-test-data ()
  (uiop:run-program (concatenate 'string
				 "mkdir -p "
				 *test-dir*))
  (uiop:run-program (concatenate 'string
				 "printf "
				 "\"" *test-file-data* "\""
				 " >"
				 *test-dir*
				 *test-file*)))

(defun clean-test-data ()
  (uiop:run-program (concatenate 'string
				 "rm -r "
				 *test-dir*)))

(defun make-ipfs-name (name)
  (intern (format nil "~{~A~^-~}"
		  (loop for token in name
		     collect (string-upcase token)))
	  'cl-ipfs-api))

(defun make-ipfs-opts (opts)
  (loop for (k v) in opts
     append (list (alexandria:make-keyword (string-upcase k)) v)))

;;; expect the same output from the ipfs cli and cl-ipfs-api
(defun is-api-cli (name args opts)
  (is (apply (make-ipfs-name name) `(,@args ,@(make-ipfs-opts opts)))
      (ipfs-command name args opts)))

(plan nil)

(subtest "init tests"
  (make-test-data))

(subtest "command expansion"
  ;; test normal
  (is-expand (cl-ipfs-api::define-command :name ("test" "command") :args ((:name "data" :required t)) :kwargs ("encoding") :description "test description")
	     (defun test-command (data &key encoding cl-ipfs-api::want-stream)
	       "test description"
	       (cl-ipfs-api:request-api "/test/command" data (list (cons "encoding" encoding)) cl-ipfs-api::want-stream nil)))
  ;; variadic/optional args
  (is-expand (cl-ipfs-api::define-command :name ("test" "command") :args ((:name "data" :required nil)) :kwargs ("encoding") :description "test description")
	     (defun test-command (cl-ipfs-api::args &key encoding cl-ipfs-api::want-stream)
	       "test description"
	       (cl-ipfs-api:request-api "/test/command" cl-ipfs-api::args (list (cons "encoding" encoding)) cl-ipfs-api::want-stream nil)))
  ;; stream output
  (is-expand (cl-ipfs-api::define-command :name ("test" "command") :args ((:name "data" :required t)) :kwargs () :description "test description" :output "stream")
	     (defun test-command (data &key cl-ipfs-api::want-stream)
	       "test description"
	       (cl-ipfs-api:request-api "/test/command" data (list (cons "encoding" "text")) cl-ipfs-api::want-stream t))))

(subtest "add"
  (is (cdr (assoc "Hash" (car (cl-ipfs-api:add *test-file-path*)) :test #'string=))
      (cadr (split-sequence:split-sequence #\Space (ipfs-command '("add") `(,*test-file-path*) nil)))))

(subtest "cat"
  (is-api-cli '("cat") `(,*test-file-hash*) nil))

;(subtest "ls")

;(subtest "refs")

;(subtest "refs local")

(subtest "block stat"
  (is-api-cli '("block" "stat") `(,*test-file-hash*) '(("encoding" "json")))
  (is-api-cli '("block" "stat") `(,*test-file-hash*) '(("encoding" "text"))))

(subtest "block get"
  (is (babel:octets-to-string (cl-ipfs-api:block-get *test-file-hash*))
      (ipfs-command '("block" "get") `(,*test-file-hash*) nil)))

;(subtest "block put")

(subtest "object new"
  (is (cl-ipfs-api:object-new nil :encoding "json")
      (ipfs-command '("object" "new") nil '(("encoding" "json"))))
  (is (cl-ipfs-api:object-new nil :encoding "text")
      (ipfs-command '("object" "new") nil '(("encoding" "text")))))

(subtest "object data"
  (is (babel:octets-to-string (cl-ipfs-api:object-data *test-file-hash*))
      (ipfs-command '("object" "data") `(,*test-file-hash*) nil)))

;(subtest "object links")

(subtest "object get"
  (is-api-cli '("object" "get") `(,*test-file-hash*) '(("encoding" "json"))))

;(subtest "object put")

(subtest "object stat"
  (is-api-cli '("object" "stat") `(,*test-file-hash*) '(("encoding" "json")))
  (is-api-cli '("object" "stat") `(,*test-file-hash*) '(("encoding" "text"))))

;(subtest "object patch")

;(subtest "file ls")

;(subtest "resolve")

;(subtest "name publish")

;(subtest "name resolve")

(subtest "dns"
  (is-api-cli '("dns") '("ipfs.io") '(("encoding" "json")))
  (is-api-cli '("dns") '("ipfs.io") '(("encoding" "text"))))

(subtest "pin add"
  (is-api-cli '("pin" "add") `(,*test-file-hash*) '(("encoding" "json")))
  (is-api-cli '("pin" "add") `(,*test-file-hash*) '(("encoding" "text"))))

(subtest "pin rm"
  (is-api-cli '("pin" "rm") `(,*test-file-hash*) '(("encoding" "json")))
  (is-api-cli '("pin" "rm") `(,*test-file-hash*) '(("encoding" "text"))))

(subtest "pin ls"
  (cl-ipfs-api:pin-add *test-file-hash*)
  (is-api-cli '("pin" "ls") nil '(("encoding" "json")))
  (is-api-cli '("pin" "ls") nil '(("encoding" "text"))))

;(subtest "repo gc")

(subtest "id"
  (is-api-cli '("id") nil '(("encoding" "json")))
  (is-api-cli '("id") nil '(("encoding" "text"))))

(subtest "bootstrap"
  (is-api-cli '("bootstrap") nil '(("encoding" "json")))
  (is-api-cli '("bootstrap") nil '(("encoding" "text"))))

;(subtest "bootstrap add")

;(subtest "bootstrap rm")

(subtest "swarm peers"
  (is-api-cli '("swarm" "peers") nil '(("encoding" "json")))
  (is-api-cli '("swarm" "peers") nil '(("encoding" "text"))))

(subtest "swarm addrs"
  (is-api-cli '("swarm" "addrs") nil '(("encoding" "json")))
  (is-api-cli '("swarm" "addrs") nil '(("encoding" "text"))))

(subtest "swarm addrs local"
  (is-api-cli '("swarm" "addrs" "local") nil '(("encoding" "json")))
  (is-api-cli '("swarm" "addrs" "local") nil '(("encoding" "text"))))

;(subtest "swarm connect")

;(subtest "swarm disconnect")

(subtest "swarm filters"
  (is-api-cli '("swarm" "filters") nil '(("encoding" "json")))
  (is-api-cli '("swarm" "filters") nil '(("encoding" "text"))))

;(subtest "swarm filters add")

;(subtest "swarm filters rm")

;(subtest "dht query")

;(subtest "dht findprovs")

;(subtest "dht findpeer")

;(subtest "dht get")

;(subtest "dht put")

;(subtest "ping")

(subtest "config"
  (is-api-cli '("config") '("Datastore.Path") '(("encoding" "json")))
  (is-api-cli '("config") '("Datastore.Path") '(("encoding" "text"))))

(subtest "config show"
  (is (babel:octets-to-string (cl-ipfs-api:config-show))
      (ipfs-command '("config" "show") nil nil)))

;(subtest "config replace")

(subtest "version"
  (is-api-cli '("version") nil '(("encoding" "json")))
  (is-api-cli '("version") nil '(("encoding" "text"))))

(subtest "clean tests"
  (clean-test-data))

(finalize)
