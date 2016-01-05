;;;; package.lisp --- Package definition for the bag-merge program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.merge
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:net.didierverna.clon

   #:rsbag

   #:rsb.tools.common)

  (:export
   #:main)

  (:documentation
   "Package definition for the bag-merge program."))
