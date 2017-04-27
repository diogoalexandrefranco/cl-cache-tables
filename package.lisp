(in-package :cl-user)

(defpackage :cache
  (:use :common-lisp)
  (:shadow)
  (:export
    #:make-cache-table
    #:cache-table-put
    #:cache-table-get
    #:cache-table-del
    #:mapcache
    #:cache-table-ttl
    #:cache-table-persist
    #:cache-table-count
    #:cache-table-get-or-fill
    #:clrcache
    #:copy-cache-table
    #:cache-table-p))
