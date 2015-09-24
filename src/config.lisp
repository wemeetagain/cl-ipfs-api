(cl:in-package #:cl-ipfs-api)

(defvar *api-path*
  "/api/v0"
  "Path prefix for all API calls")

(defvar *user-agent*
  (concatenate 'string
	       "/" (asdf:component-name (asdf:find-system :cl-ipfs-api))
	       "/" (asdf:component-version (asdf:find-system :cl-ipfs-api)))
  "Identifying user-agent string for API calls")

(defvar *scheme*
  "http"
  "Scheme for API calls")

(defvar *host*
  "localhost"
  "Hostname for API calls")

(defvar *port*
  5001
  "Port for all API calls")

(defvar *encoding*
  "json"
  "Default encoding type")
