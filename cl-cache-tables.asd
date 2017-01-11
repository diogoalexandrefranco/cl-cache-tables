(defsystem #:cl-cache-tables
           :name "cl-cache-tables"
           :version "0.0.1"
           :description "A wrapper around native hash-tables to facilitate
                        in-process caching of common lisp data structures."
           :author "Diogo Franco"
           :license "MIT"
           :serial t
           :components ((:file "package")
                        (:file "cl-cache-tables")))

(defsystem #:cl-cache-tables-tests
           :name "cl-cache-tables-tests"
           :version "0.0.1"
           :description "The tests for the cl-cache-tables system"
           :author "Diogo Franco"
           :depends-on (#:cl-cache-tables #:prove)
           :components ((:file "tests")))
