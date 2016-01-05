;;;; package.lisp --- Package definition for the bag-introspect program.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.introspect
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:iterate
   #:net.didierverna.clon

   #:rsb.tools.common

   #:rsbag.tools.common)

  (:export
   #:main)

  (:documentation
   "Package definition for the bag-introspect program."))
