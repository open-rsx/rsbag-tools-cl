;;;; play.lisp --- Implementation of the play command.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass play (file-input-mixin
                  replay-mixin
                  rsb.tools.commands:destination-mixin
                  progress-mixin
                  print-items:print-items-mixin)
    ((progress-style :initform :line))
    (:documentation
     "Publish events from a log file of recorded events.

      Replaying event flows as closely to the original as possible or
      with certain modifications can be very useful when debugging
      components, doing performance or regressions tests or testing
      the influence of parameters.

      The file formats of input files are determined based on the file
      type (extension).")))

(augment-class-documentation-with-backends 'play)

(service-provider:register-provider/class
 'command :play :class 'play)

(defun print-replay-progress (stream
                              progress index start-index end-index timestamp)
  ;; Print the progress of the current replay onto STREAM.
  (format stream "~C~A ~6,2,2F % ~9:D [~9:D,~9:D]"
          #\Return timestamp progress index start-index end-index)
  (force-output stream))

(defmethod rsb.tools.commands:command-execute ((command play)
                                               &key error-policy)
  ;; Create a reader and start the receiving and printing loop.
  (let+ (((&accessors-r/o (input-files     command-input-files)
                          (channels        command-replay-channels)
                          ((&values start-time start-index end-time end-index)
                                           command-replay-bounds)
                          (num-repetitions command-replay-num-repetitions)
                          (replay-strategy command-replay-strategy)
                          (base-uri        rsb.tools.commands:command-destination)
                          (progress-style  command-progress-style))
          command))
   (rsbag.rsb:with-open-connection
       (connection (apply #'rsbag.rsb:bag->events (first input-files) base-uri ; TODO all inputs
                          :error-policy    error-policy
                          :channels        channels
                          :replay-strategy replay-strategy
                          (append
                           (when start-time
                             (list :start-time start-time))
                           (when start-index
                             (list :start-index start-index))
                           (when end-time
                             (list :end-time end-time))
                           (when end-index
                             (list :end-index end-index))
                           (when num-repetitions
                             (list :num-repetitions num-repetitions)))))
     (log:info "~@<Connection ~A~@:>" connection)
     (rsbag.rsb:replay connection (rsbag.rsb:connection-strategy connection)
                       :progress (case progress-style
                                   (:line (curry #'print-replay-progress *info-output*)))))))
