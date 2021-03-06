;;;; package.lisp --- Package definition for the bag-cat program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015, 2016 Jan Moringen
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

  (:shadowing-import-from #:rsb.formatting
   #:create)

  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:net.didierverna.clon

   #:rsb
   #:rsb.tools.common
   #:rsb.formatting

   #:rsbag
   #:rsbag.view
   #:rsbag.rsb
   #:rsbag.tools.common)

  (:export
   #:main)

  (:documentation
   "Package definition for the bag-cat program."))
