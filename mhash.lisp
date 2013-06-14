
;;;;
;;;; Multiple key hash tables
;;;; Create a table using (make-mhash-table dimension)
;;;; The usual options from make-hash-table are provided
;;;;
;;;; Get a value from the table using (mgethash hash-table &rest keys)
;;;; E.g.
;;;; (setf (getmhash ht 'a 'b) 'ab)
;;;; (getmhash ht 'a 'b) -> (values 'ab t)
;;;; (getmhash ht 'b 'a) -> (values 'ab t)
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
		   #:getmhash
		   #:remmhash
		   #:clrmhash
		   #:mhash-table-p
		   #:mhash-count
		   #:mapmhash
		   #:mapmhash*))

(in-package #:mhash)


;; factorials using cached values up to 500 (should easily be big enough)
(let* ((max-fac 500)
	   (table (make-array max-fac)))
  (labels ((gen-fac (n)
			 (labels ((rec (n acc)
						(if (= n 0)
							acc
							(rec (1- n) (* acc n)))))
			   (rec n 1))))
	(dotimes (i max-fac)
	  (setf (svref table i) (gen-fac i)))

	(defun factorial (n)
	  "Factorial using cached values"
	  (if (< n max-fac)
		  (svref table n)
		  (gen-fac n)))))

(defun ncr (n k)
  "nCr combination function"
  (/ (factorial n)
	 (* (factorial k) (factorial (- n k)))))


(defun base-offset (num-vars degree)
  (if (< degree 0)
	  0
	  (ncr (+ num-vars degree)
		   degree)))

(defun number-terms (num-vars degree)
  (if (= degree 0)
	  1
	  (- (base-offset num-vars degree)
		 (base-offset num-vars (1- degree)))))

(defun power-offset (powers)
  (let ((degree (reduce #'+ (cdr powers))))
	(cond
	  ((or (null (cdr powers))
		   (= (+ (car powers) degree) 0))
	   0)
	  (t
	   (+ (number-terms (length powers) (1- degree))
		  (power-offset (cdr powers)))))))

(defun offset (subscripts)
  (let* ((subscripts (sort subscripts #'>=))
		 (degree (car subscripts))
		 (subscripts (maplist (lambda (subs)
								(if (cdr subs)
									(- (car subs) (cadr subs))
									(car subs)))
							  subscripts)))
	(+ (base-offset (length subscripts) (1- degree))
	   (power-offset subscripts))))

(defclass carray ()
  ((contents :reader carray-contents :initarg :contents)
   (total-size :reader carray-total-size :initarg :total-size)
   (dimensions :reader carray-dimensions :initarg :dimensions)
   (ndim :reader carray-ndims :initarg :ndims)))

(defmethod print-object ((c carray) stream)
  (print-unreadable-object (c stream)
	(format stream "CARRAY :DIMENSIONS ~A :CONTENTS ~A"
			(carray-dimensions c)
			(carray-contents c))))

(defun make-carray (size ndims &key (initial-element 0))
  (let* ((tsize (base-offset ndims size))
		 (c (make-array tsize :initial-element initial-element)))
	(make-instance 'carray
				   :contents c
				   :dimensions (loop for i below ndims collect size)
				   :ndims ndims
				   :total-size tsize)))

(defun caref (carray &rest subscripts)
  (svref (carray-contents carray)
		 (offset subscripts)))

(defun (setf caref) (value carray &rest subscripts)
  (setf (svref (carray-contents carray)
			   (offset subscripts))
		value))

(defun flat-caref (carray index)
  (svref (carray-contents carray) index))

(defun (setf flat-caref) (value carray index)
  (setf (svref (carray-contents carray) index)
		value))

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
  (let ((c (make-carray size dimension
										:initial-element nil)))
	(make-instance 'mhash-table
				   :contents c
				   :dimension dimension
				   :size size
				   :total-size (carray-total-size c)
				   :rehash-size rehash-size
				   :rehash-threshold rehash-threshold
				   :test test
				   :hash-function (if hash-function hash-function #'sxhash))))

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
  (apply #'caref (mhash-contents hash-table) subscripts))

(defun entry-test (test keys1 keys2)
  (every (lambda (k)
		   (member k keys2) :test test)
		 keys1))

(defun getmhash (hash-table &rest keys)
  "Find the entry in the MHASH-TABLE whose keys are KEYS and returns
the associated value and T as multiple objects, or return NIL and NIL
if there is no such entry. Entries can be added using SETF."
  (let ((subscripts (mhash-subscripts hash-table keys)))
	(do ((entries (apply #'caref (mhash-contents hash-table) subscripts)
				  (cdr entries)))
		((null entries)
		 (values nil nil))
	  (destructure-entry (val key-list) (car entries)
						 (if (entry-test (mhash-test hash-table)
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
	(setf (flat-caref (mhash-contents hash-table) i)
		  nil))
  hash-table)

(defun remmhash (hash-table &rest keys)
  "Remove the entry from the MHASH-TABLE associated with KEYS.
Return T if where was such an entry, or NIL if not."
  (let ((subscripts (mhash-subscripts hash-table keys))
		(found nil))
	(setf (apply #'caref (mhash-contents hash-table)
				 subscripts)
		  (mapcan (lambda (entry)
					(destructure-entry (val key-list) entry
									   (if (entry-test (mhash-test hash-table)
													   keys
													   key-list)
										   (progn
											 (decf (mhash-count hash-table))
											 (setf found t)
											 nil)
										   (list entry))))
				  (apply #'caref (mhash-contents hash-table)
						 subscripts)))
	found))

(defun mhash-resize (hash-table)
  "Increase the size of the hash table"
  (let* ((nsize (ceiling (* (mhash-size hash-table)
							(mhash-rehash-size hash-table))))
		 (contents (make-carray nsize (mhash-dimension hash-table)
								:initial-element nil)))
	(dotimes (i (mhash-total-size hash-table))
	  (dolist (entry (flat-caref (mhash-contents hash-table) i))
		(destructure-entry (val keys) entry
						   (let ((subscripts (mapcar (lambda (key)
													   (mod (funcall (mhash-function hash-table) key) nsize))
													 keys)))
							 (push entry (apply #'caref contents subscripts))))))
	(setf (mhash-contents hash-table) contents
		  (mhash-size hash-table) nsize
		  (mhash-total-size hash-table) (carray-total-size contents))
	hash-table))

(defun (setf getmhash) (value hash-table &rest keys)
  "Add or replace an entry in the hash table"
  (if (> (/ (mhash-count hash-table) (mhash-total-size hash-table))
		 (mhash-rehash-threshold hash-table))
	  (mhash-resize hash-table))
  (let ((subscripts (mhash-subscripts hash-table keys))
		(replaced nil))
	(setf (apply #'caref (mhash-contents hash-table) subscripts)
		  (mapcan (lambda (entry)
					(destructure-entry (val key-list) entry
									   (if (entry-test (mhash-test hash-table)
													   keys
													   key-list)
										   (progn
											 (setf replaced t)
											 (list (make-entry value keys)))
										   (list entry))))
				  (mhash-entry hash-table subscripts)))
	(unless replaced
	  (push (make-entry value keys)
			(apply #'caref (mhash-contents hash-table) subscripts))
	  (incf (mhash-count hash-table)))
	value))

(defun mapmhash (function hash-table)
  "For each entry in the MHASH-TABLE, call the designated two-argument function
on the value and keys of the entry. Returns the new hash table."
  (let ((h (make-mhash-table (mhash-dimension hash-table)
							 :test (mhash-test hash-table)
							 :size (mhash-size hash-table)
							 :rehash-size (mhash-rehash-size hash-table)
							 :rehash-threshold (mhash-rehash-threshold hash-table))))
	(dotimes (i (mhash-total-size hash-table))
	  (let ((entries (flat-caref (mhash-contents hash-table) i)))
		(dolist (entry entries)
		  (destructure-entry (val keys) entry
							 (setf (apply #'getmhash h keys)
								   (funcall function val keys))))))
	h))

(defun mapmhash* (function hash-table)
  "For each entry in the MHASH-TABLE, call the designated two-argument function on the value
and keys of the entry. Returns the original hash table."
  (dotimes (i (mhash-total-size hash-table))
	(let ((entries (flat-caref (mhash-contents hash-table) i)))
	  (dolist (entry entries)
		(destructure-entry (val keys) entry
						   (funcall function val keys)))))
  hash-table)

(defun print-mhash (hash-table stream)
  (format stream "~A ~A ~A ~A ~A~%"
		  (mhash-dimension hash-table)
		  (mhash-size hash-table)
		  (nth-value 2 (function-lambda-expression (mhash-test hash-table)))
		  (mhash-rehash-size hash-table)
		  (mhash-rehash-threshold hash-table))
  
  (mapmhash* (lambda (val keys)
			   (princ (cons val keys) stream)
			   (terpri stream))
			 hash-table))

(defun print-mhash-file (hash-table filename)
  (with-open-file (f filename :direction :output :if-exists :supersede)
	(print-mhash hash-table f)))

(defun load-mhash-table (stream)
  (let ((h (apply #'make-mhash-table
				  (list (read stream nil nil)
						:size (read stream nil nil)
						:test (symbol-function (read stream nil nil))
						:rehash-size (read stream nil nil)
						:rehash-threshold (read stream nil nil)
						:hash-function (symbol-function (read stream nil nil))))))
	(do ((entry (read stream nil nil) (read stream nil nil)))
		((null entry))
	  (destructure-entry (val keys) entry
						 (setf (apply #'getmhash h keys) val)))
	h))

(defun load-mhash-table-file (filename)
  (with-open-file (f filename :direction :input)
	(load-mhash-table f)))


   
		
  