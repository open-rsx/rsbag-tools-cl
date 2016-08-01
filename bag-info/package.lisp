;;;; package.lisp --- Package definition for the bag-info program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.info
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:net.didierverna.clon

   #:rsbag

   #:rsb.tools.common
   #:rsb.formatting)

  (:shadowing-import-from #:rsb.formatting
   #:create)

  (:shadow
   #:make-style-help-string)

  (:export
   #:main)

  (:documentation
   "Package definition for the bag-info program."))
