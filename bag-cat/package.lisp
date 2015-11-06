;;;; package.lisp --- Package definition for the bag-cat program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.cat
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
   #:let-plus
   #:iterate
   #:net.didierverna.clon

   #:rsb
   #:rsb.common
   #:rsb.formatting

   #:rsbag
   #:rsbag.view
   #:rsbag.rsb
   #:rsbag.common)

  (:export
   #:main)

  (:documentation
   "Package definition for the bag-cat program."))
