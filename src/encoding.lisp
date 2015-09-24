(cl:in-package #:cl-ipfs-api)

(defclass encoding ()
  ((%name :accessor encoding-name :initarg :name)))

(defgeneric parse (encoding command stream))
(defgeneric encode (encoding command stream))


(defclass json-encoding (encoding)
  ((%name :initform "json")))

(defmethod parse ((encoding json-encoding) command stream)
					;(yason:parse stream :object-as :alist))
  (jonathan:parse stream :as :alist))

(defmethod parse ((encoding json-encoding) command (stream stream))
  (loop for json = (handler-case
		       (yason:parse stream :object-as :alist)
		     (error nil nil))
     while (not (null json))
     collect json))

(defclass text-encoding (encoding)
  ((%name :initform "text")))

(defmethod parse ((encoding text-encoding) command stream)
  stream)
 ; (let ((output (make-string-output-stream)))
    ;(setf (chunga:chunked-stream-input-chunking-p stream) nil)
    ;(alexandria:copy-stream (flexi-streams:make-flexi-stream stream :external-format :utf8) output :element-type 'character)
    ;(alexandria:copy-stream stream output :element-type 'character)
    ;(get-output-stream-string output)))

(defmethod parse ((encoding text-encoding) command (stream stream))
  (loop for line = (handler-case
		       (read-line stream)
		     (error nil nil))
     while (not (null line))
     collect line))

(defclass bin-encoding (encoding)
  ((%name :initform "text")))

(defmethod parse ((encoding bin-encoding) command stream)
  (fast-io:with-fast-input (buffer stream)
    buffer))

(defclass protobuf-encoding (encoding)
  ((%name :initform "protobuf")))

(defclass xml-encoding (encoding)
  ((%name :initform "xml")))


(defparameter *encodings*
  `(("json" . ,(make-instance 'json-encoding))
    ("text" . ,(make-instance 'text-encoding))
    ("bin" . ,(make-instance 'bin-encoding))
    ("protobuf" . ,(make-instance 'protobuf-encoding))
    ("xml" . ,(make-instance 'xml-encoding)))
  "Alist of supported IPFS Encodings")

(defun get-encoding (encoding)
  (alexandria:if-let ((decoder (cdr (assoc encoding *encodings* :test #'equal))))
    decoder
    (error "No encoding found")))
