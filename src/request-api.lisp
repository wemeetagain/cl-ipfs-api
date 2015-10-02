(cl:in-package #:cl-ipfs-api)

(defun request-api (command args opts want-stream use-stream)
  (let* ((file-p (content-body-args-p args))
	 (opts `(("stream-channels" . "true")
		 ,@(unless (or (null args) file-p)
			   (args-to-opts args))
		 ,@(remove nil opts :key #'cdr)))
	 (content (when file-p
		    (get-contents args opts)))
	 (headers `(("User-Agent" . ,*user-agent*)))
	 (path (concatenate 'string *api-path* command))
	 (encoding (get-encoding (or
				  (cdr (assoc "encoding" opts :test #'string=))
				  *encoding*)))
	 (uri (quri:make-uri :scheme *scheme* :host *host* :port *port* :path path)))
    (setf (quri:uri-query-params uri) opts)
    (handler-case
	(multiple-value-bind (body status headers)
	    (dex:request uri
			 :method (if file-p
				     :post
				     :get)
			 :content content
			 :headers headers
			 :want-stream (or
				       want-stream
				       use-stream))
	  (declare (ignore status))
	  (let ((content-type (gethash "content-type" headers))
		(stream-output-p (gethash "x-stream-output" headers)))
	    (cond (want-stream
		   body)
		  (content-type
		   (parse encoding command body))
		  (use-stream
		   (if stream-output-p
		       (parse (get-encoding "text") command body)
		       (parse encoding command body)))
		  (t
		   body))))
      (dex:http-request-failed (e)
	(error (if (or want-stream use-stream)
		   (car (parse (if (cdr (assoc "encoding" opts :test #'string=))
				   encoding
				   (get-encoding "text"))
			       command (dexador.error:response-body e)))
		   (dexador.error:response-body e)))))))
