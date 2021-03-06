;;;; util.lisp --- Utilities used by commands.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

;;;

(defun coding-transform (&optional (data-access? t))
  (let ((converter (if data-access?
                       (rsb.tools.common::maybe-ensure-idl-loading-converter
                        :converters (rsb:default-converter 'nibbles:octet-vector))
                       :fundamental-null)))
    `(&from-source :converter ,converter)))

;;;

(defun print-replay-progress (stream
                              progress index start-index end-index timestamp)
  ;; Print the progress of the current replay onto STREAM.
  (when (and progress index start-index end-index)
    (format stream "~C~:[N/A~:;~:*~A~] ~6,2,2F % ~9:D [~9:D,~9:D]"
            #\Return timestamp progress index start-index end-index)
    (force-output stream)))

(defun print-ready (stream &rest args)
  (when (notany #'identity args)
    (format stream "ready~%")
    (force-output stream)))
