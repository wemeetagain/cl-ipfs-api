(cl:in-package #:cl-ipfs-api)

(defclass encoding ()
  ((%name :accessor encoding-name :initarg :name)))

(defgeneric parse (encoding command stream))
(defgeneric encode (encoding command stream))


(defclass json-encoding (encoding)
  ((%name :initform "json")))

(defmethod parse ((encoding json-encoding) command (stream string))
  (let ((stream (make-string-input-stream stream)))
    (parse encoding command stream)))

(defmethod parse ((encoding json-encoding) command (stream stream))
    (loop for json = (handler-case
		       (yason:parse stream :object-as :alist)
		     (error (c) c))
     while (not (or (null json) (typep json 'error)))
     collect json))

(defclass text-encoding (encoding)
  ((%name :initform "text")))

(defmethod parse ((encoding text-encoding) command stream)
  stream)

; TODO: rename this encoding
(defmethod parse ((encoding text-encoding) command (stream chunga:chunked-input-stream))
  (loop for buffer = (make-array 1024 :element-type '(unsigned-byte 8))
     for n of-type fixnum = (read-sequence buffer stream)
     until (zerop n)
     collect (if (= 1024 n)
		 buffer
		 (subseq buffer 0 n)) into pieces
     finally (return (apply #'concatenate '(vector (unsigned-byte 8)) pieces))))

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
