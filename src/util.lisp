(cl:in-package #:cl-ipfs-api)

;;; define-command utils

(defun make-function-name (list)
  (alexandria:symbolicate
   (format nil "~{~A~^-~}" (mapcar #'string-upcase list))))

(defun make-command-name (list)
  (format nil "~{/~A~}" list))

(defun make-args-lambda-list (args)
  (loop for arg in args
     collect (string-symbol
	      (cdr (assoc :name (alexandria:plist-alist arg))))))

(defun make-args-as-list (args)
  (let ((args (loop for arg in args
		 collect (alexandria:plist-alist arg)))
	(some-variadic (loop for arg in args
			  if (cdr (assoc :variadic args))
			  return t)))
    (cond
      ((= 0 (length args))
       nil)
      ((= 1 (length args))
       (string-symbol
	(cdr (assoc :name (car args)))))
      ((not some-variadic)
       `(list ,@(loop for arg in args
		   collect (string-symbol
			    (cdr (assoc :name arg))))))
      (t
       `(concatenate 'list
		     ,@(loop for arg in args
			  if (cdr (assoc :variadic arg))
			  collect `(if (consp ,(string-symbol
						(cdr (assoc :name arg))))
				       ,(string-symbol
					 (cdr (assoc :name arg)))
				       (list ,(string-symbol
					       (cdr (assoc :name arg)))))
			  else
			  collect `(list ,(string-symbol
					   (cdr (assoc :name arg))))))))))


(defun make-kwarg-list (kwargs)
  (loop for kwarg in kwargs
     if (equal kwarg "encoding")
     collect (list (string-symbol kwarg) '*encoding*)
     else
     collect (string-symbol kwarg)))

(defun string-symbol (string)
  (alexandria:symbolicate (string-upcase string)))

;;; request-api utils

(defun string-keyword (string)
  (unless (null string)
    (alexandria:make-keyword (string-upcase string))))

(defun symbol-downcase (keyword)
  (string-downcase (symbol-name keyword)))

(defun make-random-string ()
  (format nil "~X~X~X~X~X~X" (random 4096) (random 4096) (random 4096) (random 4096) (random 4096) (random 4096)))

(defclass ipfs-multipart-node (multipart-vfile-tree:multipart-vfile-node)
  ((%top-level-p
    :initarg :multipart-top-level-p
    :accessor multipart-top-level-p
    :initform nil)))

(defclass ipfs-multipart-directory-node (multipart-vfile-tree:multipart-vfile-directory-node ipfs-multipart-node) ())

(defmethod multipart-stream:multipart-headers ((n ipfs-multipart-directory-node))
  (list
   `("Content-Disposition" . ,(format nil "~A; filename=~S"
				      (if (multipart-top-level-p n)
					  "form-data; name=\"file\""
					  "file")
				      (multipart-vfile-tree::clean-path n)))
   `("Content-Type" . ,(format nil "multipart/mixed; boundary=~A" (multipart-vfile-tree:multipart-vfile-boundary n)))))

(defun get-contents (args opts)
  (labels ((make-multipart-dummy-file (item)
             (let ((filename (path-string:join "/tmp" (make-random-string))))
               (make-instance 'multipart-vfile-tree:multipart-vfile-node
                              :path filename
                              :base "/tmp"
                              :contents item)))
           (make-multipart (item recurse-p)
             (typecase item
               (pathname
                (multipart-vfile-tree:make-multipart-vfile-tree (namestring item) :recurse-p recurse-p :file-class 'ipfs-multipart-node :directory-class 'ipfs-multipart-directory-node))
               (string
                (if (or (uiop:file-exists-p item) (uiop:directory-exists-p item))
                    (multipart-vfile-tree:make-multipart-vfile-tree item :recurse-p recurse-p :file-class 'ipfs-multipart-node :directory-class 'ipfs-multipart-directory-node)
                    (make-multipart-dummy-file item)))
               (otherwise
                (make-multipart-dummy-file item)))))
    (let ((recursive-p (or (assoc "r" opts :test #'string=)
                           (assoc "recursive" opts :test #'string=))))
      (if (listp args)
          (mapcar (lambda (arg)
                    (make-multipart arg recursive-p))
                  args)
          (list (make-multipart args recursive-p))))))

(defun args-to-opts (args)
  (if (atom args)
      `(("arg" . ,args))
      (loop for arg in args
	 if (not (null arg))
	 collect (cons "arg" arg))))

(defun content-body-args-p (args)
  (flet ((content-body-arg-p (arg)
	   (or (pathnamep arg)
	       (subtypep (type-of arg) '(vector (unsigned-byte 8))))))
    (if (atom args)
	(content-body-arg-p args)
	(some #'content-body-arg-p args))))
