;;;; cat.lisp --- Implementation of the cat command.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

(defclass cat (file-input-mixin
               bag->events-mixin
               replay-mixin
               rsb.tools.commands:style-mixin
               rsb.tools.commands:output-stream-mixin
               print-items:print-items-mixin)
  ()
  (:documentation
   "Format events from log files onto the standard output stream.

    The file formats of input files are determined based on the file
    type (extension)."))

(augment-class-documentation-with-backends 'cat)

(service-provider:register-provider/class
 'command :cat :class 'cat)

(defmethod rsb.tools.commands:command-execute ((command cat)
                                               &key error-policy)
  ;; Gather the following things:
  ;; + input file(s)
  ;; + selected channels for replay
  ;; + temporal/index range for replay
  ;; + formatting style
  ;; Pass all of these to `bag->events' for and start the resulting
  ;; connection.
  (let+ (((&accessors-r/o (input-files     command-input-files)
                          (channels        command-replay-channels)
                          ((&values start-time start-index end-time end-index)
                                           command-replay-bounds)
                          (num-repetitions command-replay-num-repetitions)
                          (replay-strategy command-replay-strategy)
                          (style           rsb.tools.commands:command-style)
                          (stream          rsb.tools.commands:command-stream))
          command)
         (sink (lambda (datum)
                 (rsb.formatting:format-event datum style stream)))) ; TODO should be a mixin from rsb-tools-cl
    (rsbag.rsb:with-open-connection
        (connection
         (apply #'rsbag.rsb:bag->events input-files sink  ; TODO should return connection and strategy as two values
                :error-policy    error-policy
                :channels        channels
                :transform       *coding-transform*
                :replay-strategy replay-strategy
                (append (when start-time
                          (list :start-time start-time))
                        (when start-index
                          (list :start-index start-index))
                        (when end-time
                          (list :end-time end-time))
                        (when end-index
                          (list :end-index end-index))
                        (when num-repetitions
                          (list :num-repetitions num-repetitions)))))
      (log:info "~@<Replaying using connection ~A~@:>" connection)
      (rsbag.rsb:replay connection (rsbag.rsb:connection-strategy connection)))))
