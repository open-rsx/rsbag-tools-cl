;;;; protocol.lisp --- Protocol provided by the commands module.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

;;; Command service

(service-provider:define-service command
  (:documentation
   "Providers of this service define commands which can be executed by
    the bag commandline tool."))
