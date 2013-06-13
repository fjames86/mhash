====
MHASH

MHASH is a package for multiple key hash tables. The order of the keys is not significant.

* Create using (make-mhash-table dimensions &key (test #'eql) (size +min-mhash-table-size+) (rehash-size 1.5) (rehash-threshold 1) hash-function)
* Get entries using (getmhash hash-table &rest keys)
* Set entries using (setf (getmhash hash-table &rest keys) value)
* Clear using (clrmhash hash-table)
* Map using (mapmhash function hash-table), which returns a new hash table
with entries the result of applying function to the value and keys.
Also mapmhash* is provided which does the same thing but does not cons up a new
hash table.
* Any test function can be used.
* The order of the keys is not significant, so for instance (getmhash ht 'a 'b) is equivalent to (getmhash ht 'b 'a).


Frank James 2013

