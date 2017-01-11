(in-package :cache)

(defstruct
  (cache-table
    (:constructor make-cache-table
      (&key (test #'eql)
            (rehash-size 1.5)
            (rehash-threshold 0.75)
            (size 1024)
       &aux (hash (make-hash-table :test test
                                   :size size
                                   :rehash-size rehash-size
                                   :rehash-threshold rehash-threshold))))
    (:copier nil))
  test
  size
  rehash-size
  rehash-threshold
  hash)

(defun cache-table-put (key value cache-table &key (expire 0))
  "Sets key and value in cache-table. Expire is in seconds and
  defaults to 0, which means that this key will never expire.
  Returns value."
  (check-type expire number)
  (third (setf (gethash key (cache-table-hash cache-table))
               (list expire (get-internal-real-time) value))))

(defun cache-table-get (key cache-table)
  "Returns the value at key in cache-table. Also returns a second value
  that is T if the key exists, and nil if it doesn't or has expired."
  (multiple-value-bind (val exists)
    (gethash key (cache-table-hash cache-table))
    (cond ((null exists)
           (values nil nil))
          ((or (<= (first val) 0)
               (< (/ (- (get-internal-real-time) (second val))
                     internal-time-units-per-second)
                  (first val))) ;; didn't expire
           (values (third val) exists))
          (t
           (remhash key (cache-table-hash cache-table))
           (values nil nil)))))

(defun cache-table-del (key cache-table)
  "Deletes key from cache-table. Returns t if the key existed, nil otherwise."
  (multiple-value-bind (val exists)
    (gethash key (cache-table-hash cache-table))
    (cond ((null exists) nil)
          ((and (> (first val) 0)
                (> (/ (- (get-internal-real-time) (second val))
                      internal-time-units-per-second)
                   (first val)))
           (remhash key (cache-table-hash cache-table))
           nil)
          (t (remhash key (cache-table-hash cache-table))))))

(defun mapcache (function cache-table)
  "Calls fun for every existing and non-expired key value pair in cache-table.
  Similarly to maphash, returns nil."
  (maphash #'(lambda (k v)
                (if (or (<= (first v) 0)
                        (< (/ (- (get-internal-real-time) (second v))
                              internal-time-units-per-second)
                           (first v)))
                    (funcall function k (third v))
                    (remhash k (cache-table-hash cache-table))))
    (cache-table-hash cache-table)))

(defun cache-table-count (cache-table)
  "Returns the number of elements in cache-table.
  A cleared or new cache-table returns 0."
  (let ((count 0))
    (maphash #'(lambda (k v)
                (if (or (<= (first v) 0)
                        (< (/ (- (get-internal-real-time) (second v))
                              internal-time-units-per-second)
                           (first v)))
                    (incf count)
                    (remhash k (cache-table-hash cache-table))))
      (cache-table-hash cache-table))
    count))

(defun clrcache (cache-table)
  "Removes all entries from cache-table and returns that empty cache-table."
  (clrhash (cache-table-hash cache-table))
  cache-table)

(defun copy-hash-table (hash-table)
  "Helper function that copies a hash table."
  (let ((ht (make-hash-table
             :test (hash-table-test hash-table)
             :rehash-size (hash-table-rehash-size hash-table)
             :rehash-threshold (hash-table-rehash-threshold hash-table)
             :size (hash-table-size hash-table))))
    (loop for key being each hash-key of hash-table
       using (hash-value value)
       do (setf (gethash key ht) value)
       finally (return ht))))

(defun copy-cache-table (cache-table)
  "Creates a new cache-table equal to cache-table and returns it."
  (let ((copy (make-cache-table)))
    (setf (cache-table-test copy) (cache-table-test cache-table))
    (setf (cache-table-size copy) (cache-table-size cache-table))
    (setf (cache-table-rehash-size copy)
          (cache-table-rehash-size cache-table))
    (setf (cache-table-rehash-threshold copy)
          (cache-table-rehash-threshold cache-table))
    (setf (cache-table-hash copy)
          (copy-hash-table (cache-table-hash cache-table)))
    copy))

(defun cache-table-ttl (key cache-table)
  "Returns the time to live (in seconds) of the key in cache-table.
  If the key is persistent, returns -1. If it does not exist, returns nil."
  (let ((value (gethash key (cache-table-hash cache-table))))
    (cond ((null value) nil)
          ((<= (first value) 0) -1)
          ((> (/ (- (get-internal-real-time) (second value))
                 internal-time-units-per-second)
              (first value))
           (remhash key (cache-table-hash cache-table))
           nil)
          (t
            (- (+ (first value) (/ (second value) internal-time-units-per-second))
               (/ (get-internal-real-time) internal-time-units-per-second))))))

(defun cache-table-persist (key cache-table)
  "Removes the expiration from a key. Returns nil if key did not exist,
  and nil otherwise."
  (let ((value (gethash key (cache-table-hash cache-table))))
    (cond ((null value) nil)
          ((and (> (first value) 0)
                (> (/ (- (get-internal-real-time) (second value))
                      internal-time-units-per-second)
                   (first value)))
           (remhash key (cache-table-hash cache-table))
           nil)
          (t
            (setf (first (gethash key (cache-table-hash cache-table))) 0)
            (setf (second (gethash key (cache-table-hash cache-table)))
                  (get-internal-real-time))
            t))))

(defun cache-table-get-or-fill (key cache-table fun &key (expire 0))
  "Retrieves key from cache table, similarly to cache-table-get.
  When the key is not present, function is called with the key argument,
  to compute/retrieve it from another source. cache-table is then populated
  with the result and expiration of \"expire\" seconds. Returns the value
  of key. Notice that when function returns nil, that value becomes cached as
  any other value, so if you want to signal that some value couldn't be found,
  your function should throw an error"
  (multiple-value-bind (val exists)
    (gethash key (cache-table-hash cache-table))
    (cond ((or (null exists)
               (and (> (first val) 0)
                    (>= (/ (- (get-internal-real-time) (second val))
                           internal-time-units-per-second)
                        (first val))))
           (let* ((result (funcall fun key)))
              (cache-table-put key result cache-table :expire expire)
              (values result t)))
          (t (values (third val) t)))))
