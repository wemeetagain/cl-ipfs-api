(cl:in-package #:cl-user)

(defpackage #:cl-ipfs-api
  (:use #:cl)
  (:nicknames #:ipfs-api)
  ;; connection macro
  (:export #:with-ipfs-connection)
  ;; config
  (:export #:*api-path*
	   #:*user-agent*
	   #:*host*
	   #:*port*
	   #:*encoding*)
  ;; low level api
  (:export #:request-api)
  ;; ipfs api
  (:export ;; basic commands
           #:add
	   #:cat
	   #:ls
	   #:refs
	   #:refs-local
	   ;; data structure commands
	   #:block-stat
	   #:block-get
	   #:block-put
	   #:object-new
	   #:object-data
	   #:object-links
	   #:object-get
	   #:object-put
	   #:object-stat
	   #:object-patch
	   #:file-ls
	   ;; advanced commands
	   #:resolve
	   #:name-publish
	   #:name-resolve
	   #:dns
	   #:pin-add
	   #:pin-rm
	   #:pin-ls
	   #:repo-gc
	   ;; network commands
	   #:id
	   #:bootstrap
	   #:bootstrap-add
	   #:bootstrap-rm
	   #:swarm-peers
	   #:swarm-addrs
	   #:swarm-connect
	   #:swarm-disconnect
	   #:swarm-filters
	   #:swarm-filters-add
	   #:swarm-filters-rm
	   #:dht-query
	   #:dht-findprovs
	   #:dht-findpeer
	   #:dht-get
	   #:dht-put
	   #:ping
	   ;; tool commands
	   #:config
	   #:config-show
	   #:config-replace
	   #:version))
