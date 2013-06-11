
;;;; Copyright (C) 2013 Frank James
;;;;
;;;; You are granted the rights to distribute and use this software
;;;; as governed by the terms of the Lisp Lesser General Public License
;;;; (http://opensource.franz.com/preamble.html), also known as the LLGPL.

(in-package #:asdf)

(defsystem #:mhash
  :name "MHASH"
  :author "Frank James <frank.a.james@gmail.com>"
  :version "1"
  :maintainer "Frank James <frank.a.james@gmail.com>"
  :licence "Lisp Lesser General Public License (LLGPL)"
  :description "Common Lisp Multiple key hash tables."
  :long-description "MHASH is a package for multiple key hash tables."

  :components
  ((:file "mhash")))


  