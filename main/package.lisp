;;;; package.lisp --- Package definition for the main bag program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.main
  (:use
   #:cl
   #:alexandria
   #:iterate
   #:com.dvlsoft.clon

   #:rsbag

   #:rsb.common)

  (:export
   #:main)

  (:export
   #:make-static
   #:make-dynamic)

  (:documentation
   "Package definition for the main bag program."))
