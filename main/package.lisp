;;;; package.lisp --- Package definition for the main bag program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.main
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:net.didierverna.clon

   #:rsbag

   #:rsb.tools.common)

  (:export
   #:main)

  (:export
   #:make-static
   #:make-dynamic)

  (:documentation
   "Package definition for the main bag program."))
