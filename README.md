====
MHASH

MHASH is a package for multiple key hash tables.

* Create using (make-mhash-table dimensions &key (test #'eql) (size +min-mhash-table-size+) (rehash-size 1.5) (rehash-threshold 1) hash-function)
* Get entries using (mgethash hash-table &rest keys)
* Set entries using (setf (mgethash hash-table &rest keys) value)
* Clear using (clrmhash hash-table)
* Map using (mapmhash function hash-table), which returns a new hash table
with entries the result of applying function to the value and keys.
Also mapmhash* is provided which does the same thing but does not cons up a new
hash table.


Frank James 2013

