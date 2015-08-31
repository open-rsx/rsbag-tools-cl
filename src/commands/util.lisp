;;;; util.lisp --- Utilities used by commands.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

(defun print-replay-progress (stream
                              progress index start-index end-index timestamp)
  ;; Print the progress of the current replay onto STREAM.
  (format stream "~C~A ~6,2,2F % ~9:D [~9:D,~9:D]"
          #\Return timestamp progress index start-index end-index)
  (force-output stream))
