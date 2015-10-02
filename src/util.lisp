(cl:in-package #:cl-ipfs-api)

;;; define-command utils

(defun make-function-name (list)
  (alexandria:symbolicate
   (format nil "~{~A~^-~}" (mapcar #'string-upcase list))))

(defun make-command-name (list)
  (format nil "~{/~A~}" list))

(defun make-arg-list (args)
  (loop for arg in args
     collect (string-symbol
	      (cdr (assoc :name (alexandria:plist-alist arg))))))

(defun make-args-to-arg-list (args)
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

;;; close enough for now
(defun get-contents (args opts)
  (alexandria:if-let ((recursive-p (or (assoc "r" opts :test #'string=)
				       (assoc "recursive" opts :test #'string=))))
    (labels ((all-files (pathname list)
	       (cond
		 ((uiop:directory-pathname-p pathname)
		  (append (loop for subfile in (uiop:directory-files pathname)
			     collect (cons (pathname-name subfile) subfile))
			  (loop for subdirectory in (uiop:subdirectories pathname)
			     append (all-files subdirectory nil))
			  list))
		  ((pathnamep args)
		   (cons (cons (pathname-name args) args) list))
		  (t list))))
      (all-files args nil))
    (cond
      ((pathnamep args)
       (list (cons (pathname-name args) args)))
      ((subtypep (type-of args) '(vector (unsigned-byte 8)))
       (list (cons "path" args))))))

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
