;;;; introspect.lisp --- Implementation of the introspect command.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

(defun introspection-channel? (channel)
  (starts-with-subseq
   (load-time-value
    (rsb:scope-string rsb.introspection:+introspection-participants-scope+)
    t)
   (channel-name channel)))

(defclass introspect (file-input-mixin
                      bag->events-mixin
                      rsb.tools.commands:style-mixin
                      rsb.tools.commands:output-stream-mixin
                      progress-mixin
                      print-items:print-items-mixin)
  ((rsb.tools.commands::style-service :allocation :class
                                      :initform   'rsb.formatting.introspection::style)
   (progress-style                    :initform   :line))
  (:default-initargs
   :channels #'introspection-channel?)
  (:documentation
   "Gather introspection information from log files.

    The file formats of input files are determined based on the file
    type (extension)."))

(augment-class-documentation-with-backends 'introspect)

(service-provider:register-provider/class
 'command :introspect :class 'introspect)

(defmethod rsb.tools.commands:command-execute ((command introspect)
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
                          (style           rsb.tools.commands:command-style)
                          (stream          rsb.tools.commands:command-stream)
                          (progress-style  command-progress-style))
          command)
         (database (make-instance 'rsb.introspection::remote-introspection-database))
         ((&flet process-event (event)
            (when (and (not (rsb:event-method event))
                       (not (typep (rsb:event-data event)
                                   'rsb.introspection:bye)))
              (rsb.ep:handle database event)))))
    (rsbag.rsb:with-open-connection
        (connection
         (apply #'rsbag.rsb:bag->events input-files #'process-event
                :error-policy    error-policy
                :channels        channels
                :transform       `(&from-source
                                   :converter ,rsb.introspection::*introspection-all-converters*)
                :replay-strategy :as-fast-as-possible
                (append (when start-time
                          (list :start-time start-time))
                        (when start-index
                          (list :start-index start-index))
                        (when end-time
                          (list :end-time end-time))
                        (when end-index
                          (list :end-index end-index)))))
      (log:info "~@<Replaying using connection ~A~@:>" connection)
      (rsbag.rsb.replay:replay
       connection (rsbag.rsb:connection-strategy connection)
       :progress (case progress-style
                   (:line (curry #'print-replay-progress *info-output*)))))
    (case progress-style
      (:line (fresh-line *info-output*)))

    (setf (rsb.formatting.introspection::style-database style) database)
    (rsb.formatting:format-event :dummy style stream)))
