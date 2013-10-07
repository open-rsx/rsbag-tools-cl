;;;; package.lisp --- Package definition for the bag-info program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.info
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:com.dvlsoft.clon

   #:rsbag

   #:rsb.common
   #:rsb.formatting)

  (:export
   #:main)

  (:documentation
   "Package definition for the bag-info program."))
