;;;; package.lisp --- Package definition for the bag-record program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.record
  (:shadowing-import-from #:rsbag
   #:direction

   #:meta-data
   #:meta-data-count
   #:meta-data-keys
   #:meta-data-values
   #:meta-data-plist
   #:meta-data-alist)

  (:use
   #:cl
   #:alexandria
   #:iterate
   #:let-plus
   #:com.dvlsoft.clon

   #:rsb
   #:rsb.patterns
   #:rsb.common

   #:rsbag
   #:rsbag.rsb)

  (:export
   #:main)

  (:documentation
   "Package definition for the bag-record program."))
