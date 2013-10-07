;;;; package.lisp --- Package definition for common module.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :rsbag.common
  (:use
   :cl
   :alexandria
   :let-plus

   :com.dvlsoft.clon

   :rsbag.rsb.replay

   :rsb.common)

  ;; Commandline options
  (:export
   :make-replay-options
   :process-bounds-options)

  ;; Help text generation
  (:export
   :make-replay-strategy-help-string)

  (:documentation
   "This package contains some common utility functions for RSBag:
+ Commandline option definition and processing
+ Help text generation"))
