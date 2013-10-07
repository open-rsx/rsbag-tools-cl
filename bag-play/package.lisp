;;;; package.lisp --- Package definition for the bag-play program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.play
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
   #:com.dvlsoft.clon

   #:rsb
   #:rsb.common

   #:rsbag
   #:rsbag.rsb
   #:rsbag.common)

  (:export
   #:main)

  (:documentation
   "Package definition for the bag-play program."))
