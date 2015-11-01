(cl:in-package #:cl-ipfs-api)

(defmacro define-command (&key name args kwargs input output description &allow-other-keys)
  `(defun ,(make-function-name name)
       (,@(make-args-lambda-list args) &key ,@(make-kwarg-list kwargs) want-stream)
     ,@(when description (list description))
     (request-api
      ,(make-command-name name)
      ,(when (not (string= input "stream"))
         (make-args-as-list args))
      ,(when (string= input "stream")
         (make-args-as-list args))
      (list
       ,@(when (string= output "stream") '((cons "encoding"  "text")))
       ,@(loop for kwarg in kwargs
	    collect `(cons ,kwarg  ,(string-symbol kwarg))))
      want-stream
      ,(when output t))))					 

#.(let ((definitions (jonathan:parse
		      (alexandria:read-file-into-string
		       (asdf:component-pathname (asdf:find-component '("cl-ipfs-api" "src" "definitions.json") nil)))
		      :keyword-normalizer #'string-upcase
		      :normalize-all t)))
    (loop for definition in definitions
       collect `(define-command ,@definition) into commands
       finally (return `(progn ,@commands))))
