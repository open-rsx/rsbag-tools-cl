;;;; package.lisp --- Package definition for common module.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.common
  (:use
   #:cl
   #:alexandria
   #:let-plus
   #:more-conditions

   #:net.didierverna.clon

   #:rsbag.rsb.replay

   #:rsb.tools.common)

  ;; Commandline options
  (:export
   #:make-replay-options
   #:process-bounds-options

   #:make-index-timestamp-option
   #:make-channel-allocation-option)

  ;; Help text generation
  (:export
   #:make-replay-strategy-help-string
   #:make-channel-strategy-help-string)

  (:documentation
   "This package contains some common utility functions for RSBag:
    + Commandline option definition and processing
    + Help text generation"))
