;;;; redump.lisp --- Redump command.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

(service-provider:register-provider/class
 'command :redump :class 'rsb.tools.commands:redump)
