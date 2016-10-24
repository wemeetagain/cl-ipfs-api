(cl:in-package #:cl-ipfs-api)

(defun request-api (command args files opts want-stream use-stream)
  (flet ((handle-request (uri headers content encoding)
           (handler-case
               (multiple-value-bind (body status headers uri connection)
                   (dex:request uri
                                :method (if content
                                            :post
                                            :get)
                                :content content
                                :headers headers
                                :use-connection-pool nil
                                :want-stream (or
                                              want-stream
                                              nil))
                                        ;cuse-stream))
                 (declare (ignore status uri))
                 (close connection)
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
    (declare (inline handle-request))
    (let* ((file-p files)
           (path (concatenate 'string *api-path* command))
           (encoding (get-encoding (or
                                    (cdr (assoc "encoding" opts :test #'string=))
                                    *encoding*)))
           (uri (quri:make-uri :scheme *scheme* :host *host* :port *port* :path path))
           (headers `(("User-Agent" . ,*user-agent*)
                      ("Connection" . "keep-alive")))
           (opts `(("stream-channels" . "true")
                   ,@(remove nil opts :key #'cdr))))
      (if file-p
          (let* ((content (get-contents files opts))
                 (boundary (multipart-stream:make-boundary))
                 (headers `(("Content-Disposition" . "form-data: name=\"files\"")
                            ("Content-Type" . ,(format nil "multipart/form-data; boundary=~A" boundary))
                            ,@headers)))
            (mapc (lambda (file)
                    (setf (multipart-top-level-p file) t))
                  content)
            (setf (quri:uri-query-params uri) opts)
            (with-open-stream (content (apply #'multipart-stream:make-multipart-stream boundary content))
              (handle-request uri headers content encoding)))
          (let ((opts (append (args-to-opts args) opts)))
            (setf (quri:uri-query-params uri) opts)
            (handle-request uri headers nil encoding))))))
