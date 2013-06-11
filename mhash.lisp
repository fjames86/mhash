
;;;;
;;;; Multiple key hash tables
;;;; Create a table using (make-mhash-table dimension)
;;;; The usual options from make-hash-table are provided
;;;;
;;;; Get a value from the table using (mgethash hash-table &rest keys)
;;;; E.g.
;;;; (setf (mgethash ht 'a 'b) 'ab)
;;;; (mgethash ht 'a 'b) -> (values 'ab t)
;;;; (mgethash ht 'b 'a) -> (values nil nil)
;;;; etc.
;;;; 
;;;; Copyright (C) 2013 Frank James
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(defpackage #:mhash
  (:use #:cl)
  (:export #:make-mhash-table
		   #:mgethash
		   #:mremhash
		   #:clrmhash
		   #:mhash-table-p
		   #:mhash-count
		   #:mapmhash
		   #:mapmhash*))

(in-package #:mhash)
		   
(defconstant +min-mhash-table-size+ 4)

(defclass mhash-table ()
  ((contents :accessor mhash-contents :initarg :contents)
   (dimension :reader mhash-dimension :initarg :dimension)
   (size :accessor mhash-size :initarg :size)
   (total-size :accessor mhash-total-size :initarg :total-size)
   (count :accessor mhash-count :initform 0)
   (rehash-size :reader mhash-rehash-size :initarg :rehash-size)
   (thresold :reader mhash-rehash-threshold :initarg :rehash-threshold)
   (hash-function :reader mhash-function :initarg :hash-function)
   (test :reader mhash-test :initarg :test)))

(defmethod print-object ((h mhash-table) stream)
  (print-unreadable-object (h stream)
	(format stream "MHASH-TABLE :DIMENSION ~A :COUNT ~A :SIZE ~A"
			(mhash-dimension h)
			(mhash-count h)
			(mhash-size h))))

(defun make-mhash-table (dimension &key
						 (test #'eql)
						 (size +min-mhash-table-size+)
						 (rehash-size 1.5)
						 (rehash-threshold 1)
						 hash-function)
  "Make an mhash object."
  (make-instance 'mhash-table
				 :contents (make-array (loop for i below dimension collect size)
									   :initial-element nil)
				 :dimension dimension
				 :size size
				 :total-size (expt size dimension)
				 :rehash-size rehash-size
				 :rehash-threshold rehash-threshold
				 :test test
				 :hash-function (if hash-function hash-function #'sxhash)))

(defun mhash-subscripts (hash-table keys)
  "Get the subscripts for the key list"
  (let ((n (mhash-size hash-table)))
	(mapcar (lambda (key)
			  (mod (funcall (mhash-function hash-table) key) n))
			keys)))

(defun make-entry (value keys)
  (cons value keys))

(defmacro destructure-entry ((value keys) entry &body body)
  `(destructuring-bind (,value . ,keys) ,entry
	 (declare (ignorable ,value ,keys))
	 ,@body))

(defun mhash-entry (hash-table subscripts)
  (apply #'aref (mhash-contents hash-table) subscripts))

(defun mgethash (hash-table &rest keys)
  "Find the entry in the MHASH-TABLE whose keys are KEYS and returns
the associated value and T as multiple objects, or return NIL and NIL
if there is no such entry. Entries can be added using SETF."
  (let ((subscripts (mhash-subscripts hash-table keys)))
	(do ((entries (apply #'aref (mhash-contents hash-table) subscripts)
				  (cdr entries)))
		((null entries)
		 (values nil nil))
	  (destructure-entry (val key-list) (car entries)
		(if (every (mhash-test hash-table)
				   keys
				   key-list)
			(return (values val t)))))))

(defun mhash-table-p (obj)
  "Predicate for an mhash table"
  (typep obj 'mhash-table))

(defun clrmhash (hash-table)
  "This removes all the entries from the MHASH-TABLE and returns the
hash table itself."
  (dotimes (i (mhash-total-size hash-table))
	(setf (row-major-aref (mhash-contents hash-table)
						  i)
		  nil))
  hash-table)

(defun mremhash (hash-table &rest keys)
  "Remove the entry from the MHASH-TABLE associated with KEYS.
Return T if where was such an entry, or NIL if not."
  (let ((subscripts (mhash-subscripts hash-table keys))
		(found nil))
	(setf (apply #'aref (mhash-contents hash-table)
				 subscripts)
		  (mapcan (lambda (entry)
					(destructure-entry (val key-list) entry
					  (if (every (mhash-test hash-table)
								 keys
								 key-list)
						  (progn
							(decf (mhash-count hash-table))
							(setf found t)
							nil)
						  (list entry))))
				  (apply #'aref (mhash-contents hash-table)
						 subscripts)))
	found))

(defun mhash-resize (hash-table)
  "Increase the size of the hash table"
  (let* ((nsize (ceiling (* (mhash-size hash-table)
						   (mhash-rehash-size hash-table))))
		 (contents (make-array (loop for i below (mhash-dimension hash-table)
								   collect nsize)
							  :initial-element nil)))
	(dotimes (i (mhash-total-size hash-table))
	  (dolist (entry (row-major-aref (mhash-contents hash-table) i))
		(destructure-entry (val keys) entry
		  (let ((subscripts (mapcar (lambda (key)
									  (mod (funcall (mhash-function hash-table) key) nsize))
									keys)))
			(setf (apply #'aref contents subscripts)
				  entry)))))
	(setf (mhash-contents hash-table) contents
		  (mhash-size hash-table) nsize
		  (mhash-total-size hash-table) (expt (mhash-dimension hash-table)
											  nsize))
	hash-table))

(defun (setf mgethash) (value hash-table &rest keys)
  "Add or replace an entry in the hash table"
  (if (> (/ (mhash-count hash-table) (mhash-total-size hash-table))
		 (mhash-rehash-threshold hash-table))
	  (mhash-resize hash-table))
  (let ((subscripts (mhash-subscripts hash-table keys))
		(replaced nil))
	(setf (apply #'aref (mhash-contents hash-table) subscripts)
		  (mapcan (lambda (entry)
					(destructure-entry (val key-list) entry
					  (if (every (mhash-test hash-table)
								 keys
								 key-list)
						  (progn
							(setf replaced t)
							(list (make-entry value keys)))
						  (list entry))))
				  (mhash-entry hash-table subscripts)))
	(unless replaced
	  (push (make-entry value keys)
			(apply #'aref (mhash-contents hash-table) subscripts))
	  (incf (mhash-count hash-table)))
	hash-table))

(defun mapmhash (function hash-table)
  "For each entry in the MHASH-TABLE, call the designated two-argument function on the value and keys of the entry. Return NIL."
  (let ((h (make-mhash-table (mhash-dimension hash-table)
							 :test (mhash-test hash-table)
							 :size (mhash-size hash-table)
							 :rehash-size (mhash-rehash-size hash-table)
							 :rehash-threshold (mhash-rehash-threshold hash-table))))
	(dotimes (i (mhash-total-size hash-table))
	  (let ((entries (row-major-aref (mhash-contents hash-table) i)))
		(dolist (entry entries)
		  (destructure-entry (val keys) entry
			(setf (apply #'mgethash h keys)
				  (funcall function val keys))))))
	h))

(defun mapmhash* (function hash-table)
  "For each entry in the MHASH-TABLE, call the designated two-argument function on the value and keys of the entry. Return NIL."
  (dotimes (i (mhash-total-size hash-table))
	(let ((entries (row-major-aref (mhash-contents hash-table) i)))
	  (dolist (entry entries)
		(destructure-entry (val keys) entry
		  (funcall function val keys)))))
  hash-table)



		
   
		
  