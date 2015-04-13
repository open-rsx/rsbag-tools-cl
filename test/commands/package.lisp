;;;; package.lisp --- Package definition for unit tests of the commands module.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag.tools.commands.test
  (:use
   #:cl
   #:let-plus
   #:more-conditions

   #:lift

   #:rsbag.tools.commands)

  (:export
   #:rsbag-tools-commands-root)

  (:documentation
   "This package contains unit tests for the commands module."))

(cl:in-package #:rsbag.tools.commands.test)

(deftestsuite rsbag-tools-commands-root ()
  ()
  (:documentation
   "Root unit test suite for the commands module."))
