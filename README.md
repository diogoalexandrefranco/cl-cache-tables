# cl-cache-tables
cl-cache-tables is a small, portable, dependency-free library that
facilitates caching of data such as lisp data structures.

It uses the native hash-tables of your lisp implementation under the hood, but
implements an api that handles caching stuff such as key expiration, with little
aditional overhead.

It works at least on sbcl, ecl, ccl, abcl and clisp.

* [How do i use it?](#how-do-i-use-it)
* [Example](#example)
* [API](#api)
* [Contributing](#contributing)
* [License](#license)

## How do i use it?
This section assumes you use quicklisp. If you don't, you should! Download and
learn about it [here](https://www.quicklisp.org/beta/).

Once you have quicklisp loaded, simply do:  
```lisp
(ql:quickload :cl-cache-tables)
```
And it's all up and running. To run the tests do:
```lisp
(ql:quickload :cl-cache-tables-tests)
```
Please report if any tests fail in your Common Lisp implementation.

## Example
```lisp
> (ql:quickload :cl-cache-tables)
(:CL-CACHE-TABLES)  

> (use-package :cache)
T  

> (defvar *cache* (make-cache-table :test #'equal))
*cache*

> (progn
    (cache-table-put "key" "value" *cache* :expire 1)
    (print (cache-table-get "key" *cache*))
    (sleep 1)
    (print (cache-table-get "key" *cache*)))
"value"
nil
```

## API
#### (make-cache-table &key (test #'eql) (size 1024) (rehash-size 1.5) (rehash-threshold 0.75))
make-cache-table returns a newly created cache-table. The argument list is the same
as in the native constuctor make-hash-table.
```lisp
(defvar *cache* (make-cache-table :test #'equal))
```

#### (cache-table-put key value cache-table &key (expire 0))
cache-table-put populates the cache-table with a new key and value. The expire
parameter is the time in seconds that the key will be present, where 0 means
that it does not expire. Expire defaults to 0. cache-table-put returns the
value that was set.
```lisp
(cache-table-put "some-persistent-key" "persistent" *cache* :expire 0) ;; "persistent"
(cache-table-put "some-temporary-key" "temporary" *cache* :expire 10) ;; "temporary"
```

#### (cache-table-get key cache-table)
cache-table-get retrieves a value from key in cache-table.
Returns two values, similarly to gethash: The first one is the
value stored at key, and the second one is a generalized boolean
indicating if the key exists (this allows us to distinguish the case
where we store nil at a cache-table key).
```lisp
(cache-table-get "some-persistent-key" *cache*)
"persistent"
T
```

#### (cache-table-get-or-fill key cache-table fun &key (expire 0))
Retrieves key from cache table, similarly to cache-table-get.
When the key is not present (or has expired), fun is called with the key
argument, to compute/retrieve it from another source. cache-table is then
populated with the result and an expiration of "expire" seconds. Returns the
value of key. Notice that when function returns nil, that value becomes cached
as any other value, so if you want to signal that some value couldn't be found,
your function should signal an error."
```lisp
(defvar *cache* (make-hash-table :test #'equal)) ;; cache-table

(cache-table-get-or-fill "my-key" *cache* #'(lambda (key)
                                              (retrieve-from-mongo key)) :expire 10)
"some-value" ;; retrieved from mongodb

(cache-table-get-or-fill "my-key" *cache* #'(lambda (key)
                                              (retrieve-from-mongo key)) :expire 10)
"some-value" ;; retrieved from cache-table if less than 10 seconds elapsed, else retrieved from mongo.
```

#### (cache-table-del key cache-table)
cache-table-del deletes a key from the cache-table. Returns t if the key
existed, nil otherwise.
```lisp
(cache-table-del "some-non-existing-key" *cache*)
NIL
```

#### (mapcache function cache-table)
Similarly to maphash, receives a function of two arguments (key value),
and applies it to every key-value pair in cache-table. Always returns nil.
```lisp
(mapcache #'(lambda (key value) (print (cons key value))) *cache*)
("come-persistent-key" . "persistent")
("some-temporary-key" . "temporary")
NIL
```

#### (cache-table-ttl key cache-table)
cache-table-ttl returns the amount of time in seconds that a key still has
before expiring. If the key is persistent, this function returns -1. If it does
not exist, it returns nil.
```lisp
(cache-table-ttl "some-temporary-key" *cache*)
8

(cache-table-ttl "some-persistent-key" *cache*)
-1

(cache-table-ttl "some-non-existing-key" *cache*)
NIL
```

#### (cache-table-persist key cache-table)
cache-table-persist turns a temporary key to a persistent one, by removing
the expiration. Returns T if key exists, NIL otherwise.
```lisp
(cache-table-persist "some-temporary-key" *cache*)
T
(cache-table-ttl "a-non-existing-key" *cache*)
NIL
```

#### (cache-table-count cache-table)
Returns the number of key-value pairs in a cache-table.
```lisp
(cache-table-count *cache*)
345
```

#### (clrcache cache-table)
clrcache removes all entries from cache-table
and returns that empty cache-table.
```lisp
(clrcache *cache*) ;; cash-table
(cache-table-count *cache*) ;; 0
```

#### (copy-cache-table cache-table)
copy-cache-table creates a new cache equal
to cache-table and returns it.
```lisp
(copy-cache-table *cache*) ;; new cache-table equal to cache-table
```

#### (cache-table-p cache-table)
cache-table-p is a predicate that checks if cache-table if indeed
a cache-table. Returns T if is, NIL otherwise.
```list
(cache-table-p *cache*)
T
(cache-table-p 3)
NIL
```

## Contributing
If you have any suggestions, bug reports, etc, please fill in an issue
describing it. If you have the time and want to contribute, that is even better!
Submit some tests too, let's try and keep coverage up.

## License
MIT
