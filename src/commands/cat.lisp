;;;; cat.lisp --- Implementation of the cat command.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

(defclass cat (file-input-mixin
               bag->events-mixin
               rsb.tools.commands:filter-mixin
               replay-mixin
               rsb.tools.commands:style-mixin
               rsb.tools.commands:output-stream-mixin
               progress-mixin
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
                          (filters         rsb.tools.commands:command-filters)
                          (replay-strategy command-replay-strategy)
                          (style           rsb.tools.commands:command-style)
                          (stream          rsb.tools.commands:command-stream)
                          (progress-style  command-progress-style))
          command)
         (access? (rsb.ep:access? (list* style filters) :data :read))
         (sink    (lambda (timestamp datum) ; TODO should be a mixin from rsb-tools-cl
                    (declare (ignore timestamp))
                    (if (typep datum 'rsb:event)
                        (rsb.formatting:format-event datum style stream)
                        (rsb.formatting:format-payload
                         datum (rsb.formatting:style-payload-style style) stream)))))
    (rsbag.rsb:with-open-connection
        (connection
         (apply #'rsbag.rsb:bag->events input-files sink  ; TODO should return connection and strategy as two values
                :error-policy    error-policy
                :channels        channels
                :filters         filters
                :transform       (coding-transform access?)
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
      (rsbag.rsb.replay:replay
       connection (rsbag.rsb:connection-strategy connection)
       :progress (case progress-style
                   (:line  (curry #'print-replay-progress *info-output*))
                   (:ready (curry #'print-ready *info-output*)))))))
