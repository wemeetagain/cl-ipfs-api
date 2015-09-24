(cl:in-package #:cl-user)

(defpackage #:cl-ipfs-api-test
  (:use #:cl #:prove))
(in-package #:cl-ipfs-api-test)

(defun ipfs-command (name args opts)
  (uiop:run-program (concatenate 'string
				 "ipfs "
				 (format nil "~{~A ~}" name)
				 (format nil "~{~A ~}"args)
				 (format nil "~:{--~A ~A ~}" opts))
		    :output :string))

(defvar *test-dir* "testdir/")
(defvar *test-file* "file1.txt")
(defvar *test-file-data* "hello test\n")

(defun make-test-data ()
  (uiop:run-program (concatenate 'string
				 "mkdir "
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

(plan nil)

(subtest "init tests"
  (make-test-data))

(subtest "clean tests"
  (clean-test-data))

(finalize)
