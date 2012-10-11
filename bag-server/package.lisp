;;;; package.lisp --- Package definition for the bag-server program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.server
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:com.dvlsoft.clon

   #:rsb.common

   #:rsbag
   #:rsbag.rsb)

  (:export
   #:main)

  (:documentation
   "Package definition for the bag-server program."))
