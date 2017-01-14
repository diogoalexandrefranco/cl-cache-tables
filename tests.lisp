(in-package :cache)

(prove:plan 5)

(prove:subtest "Simple expiration"
  (let ((cache (make-cache-table :test #'equal)))
    (cache-table-put "key" "value" cache :expire 0.5)
    (prove:is (cache-table-count cache) 1 "Count after put.")
    (prove:is (cache-table-get "key" cache) "value" "Get after put.")
    (sleep 0.6)
    (prove:is (cache-table-count cache) 0 "Count after key expired.")
    (prove:ok (not (cache-table-get "key" cache)) "Get after key expired.")))

(prove:subtest "Mapcache with expired keys"
  (let ((cache (make-cache-table :test #'equal))
        (list-before nil)
        (list-after nil))
    (cache-table-put "key1" "value" cache :expire 0.5)
    (cache-table-put "key2" "value" cache :expire 2)
    (cache-table-put "key3" "value" cache :expire 0)
    (cache-table-put "key4" "value" cache)
    (mapcache #'(lambda (k v) (push k list-before)) cache)
    (sleep 0.6)
    (mapcache #'(lambda (k v) (push k list-after)) cache)
    (prove:ok (not (set-difference list-before '("key1" "key2" "key3" "key4")
                    :test #'string=)) "Mapcache before expiration.")
    (prove:ok (not (set-difference list-after '("key2" "key3" "key4")
                    :test #'string=)) "Mapcache after expiration.")))

(prove:subtest "Copy cache"
  (let ((cache1 (make-cache-table :test #'equal))
        (cache2 nil))
    (cache-table-put "key" "value" cache1)
    (setf cache2 (copy-cache-table cache1))
    (prove:is (cache-table-count cache1) 1 "Count on original")
    (prove:is (cache-table-count cache2) 1 "Count on copy")
    (cache-table-put "second key" "value" cache1)
    (prove:is (cache-table-count cache1) 2 "Count on original after modification.")
    (prove:is (cache-table-count cache2) 1 "Count on copy after modifying original.")))

(prove:subtest "Other API methods"
  (let ((cache (make-cache-table :test #'equal)))
    (cache-table-put "key" "value" cache :expire 2)
    (prove:ok (> (cache-table-ttl "key" cache) 0) "Time to live greater than zero.")
    (prove:ok (cache-table-persist "key" cache) "Persist key.")
    (prove:is (cache-table-ttl "key" cache) -1 "Ttl after persisting key.")
    (prove:ok (not (cache-table-ttl "non-existing" cache)) "Ttl of non-existing key.")
    (prove:ok (cache-table-p cache) "Type of cache-table")
    (prove:is (cache-table-count cache) 1 "Count after the previous modifications.")
    (cache-table-put "key2" "value" cache :expire 0)
    (prove:is (cache-table-count cache) 2 "Count after another insertion.")
    (prove:ok (cache-table-del "key" cache) "Delete key.")
    (prove:is (cache-table-count cache) 1 "Count after deletion.")
    (prove:ok (cache-table-p (clrcache cache)) "Clear cache table.")
    (prove:is (cache-table-count cache) 0 "Count after clearing cache table.")))

(prove:subtest "Cache-table-get-or-fill"
  (let ((cache (make-cache-table :test #'equal))
        (lambda-runs 0))
    (loop for i from 0 to 4 do
      (prove:is (cache-table-get-or-fill "my-key" cache
                  #'(lambda (key) (incf lambda-runs) "my-value") :expire 1)
                "my-value" "All of these should return my-value."))
    (prove:is lambda-runs 1 "Lambda ran only once.")
    (sleep 1)
    (prove:is (cache-table-get-or-fill "my-key" cache
                #'(lambda (key) (incf lambda-runs) "my-value") :expire 1)
              "my-value" "My-value again.")
    (prove:is lambda-runs 2 "Lambda ran twice")
    (prove:is (cache-table-get-or-fill "my-key" cache
                #'(lambda (key) (incf lambda-runs) "my-value") :expire 1)
              "my-value" "My-value again.")
    (prove:is lambda-runs 2 "Lambda still ran only twice")))
