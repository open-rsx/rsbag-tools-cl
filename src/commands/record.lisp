;;;; record.lisp --- Implementation of the record command.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

(defclass record (rsb.tools.commands:source-mixin
                  file-output-mixin
                  index-timestamp-mixin
                  progress-mixin
                  print-items:print-items-mixin)
  ((progress-style     :initform :entries)
   (channel-allocation :initarg  :channel-allocation
                       :reader   record-channel-allocation
                       :accessor record-%channel-allocation
                       :initform (rsbag.rsb:make-channel-strategy :scope-and-type)
                       :documentation
                       "TODO" #+later (make-channel-strategy-help-string :show show)
                           #+later (:short-name    "a"
                                    :argument-name "SPEC"))
   (filters            :initarg  :filters
                       :type     list
                       :reader   record-filters
                       :initform '()
                       :documentation
                       "TODO" #+later (make-filter-help-string :show show)
                           #+later (:short-name    "f"
                                    :argument-name "SPEC"))
   (flush-strategy     :initarg  :flush-strategy
                       :reader   record-flush-strategy
                       :accessor record-%flush-strategy
                       :initform nil
                       :documentation
                       "TODO" #+later (make-flush-strategy-help-string :show show)
                           #+later (:argument-name "SPEC"))
   (control-uri        :initarg  :control-uri
                       :type     (or null puri:uri)
                       :reader   record-control-uri
                       :initform nil
                       :documentation
                       "A URI specifying the root scope and transport
                        configuration of an RPC server exposing
                        methods which allow controlling the recording
                        process. Currently, the following methods are
                        provided:

                        isstarted : void -> bool

                          Return true if a log file has been opened
                          for recording and recording is currently in
                          progress and false otherwise.

                        start : void -> void

                          Start recording or restart after it has been
                          stopped. Only applicable if a bag has been
                          opened.

                        stop : void -> void

                          Stop recording allowing it to be restarted
                          later. Only applicable if a bag has been
                          opened.

                        isopen : void -> string or false

                          If a log file has been opened for recording,
                          return its path as a string. Otherwise
                          return false.

                        open : string -> void

                          Open the specified file to record into
                          it. Does not start recording. Only
                          applicable if not bag is currently open.

                        close : void -> void

                          Close the current bag. Only applicable if a
                          bag is open.

                        terminate : void -> void

                          Terminate the recording process and the
                          program."
                       #+later (:short-name "c"
                                            :argument-name "URI")))
  (:default-initargs
   :uris        (missing-required-initarg 'record :uris)
   :output-file nil)
  (:documentation
   "Capture events in a running system and store them into a log file
    for later analysis, further processing or replay.

    Capture events being exchanged on the scopes designated by URIS
    and store them in the specified output file. For each URI, one or
    more channels may be created in the output file (depending on the
    channel allocation strategy). These channels store the events
    exchanged in the corresponding RSB channels.

    The file formats of output files are determined based on the file
    type (extension)."))

(augment-class-documentation-with-backends 'record)

(service-provider:register-provider/class
 'command :record :class 'record)

(defmethod shared-initialize :before
    ((instance   record)
     (slot-names t)
     &key
     output-file
     (channel-allocation      nil channel-allocation-supplied?)
     (channel-allocation-spec nil channel-allocation-spec-supplied?)
     (flush-strategy          nil flush-strategy-supplied?)
     (flush-strategy-spec     nil flush-strategy-spec-supplied?)
     (control-uri             nil control-uri-supplied?))
  (declare (ignore control-uri))
  (when (and channel-allocation-supplied? channel-allocation-spec-supplied?)
    (incompatible-initargs 'record
                           :channel-allocation      channel-allocation
                           :channel-allocation-spec channel-allocation-spec))
  (when (and flush-strategy-supplied? flush-strategy-spec-supplied?)
    (incompatible-initargs 'record
                           :flush-strategy      flush-strategy
                           :flush-strategy-spec flush-strategy-spec))
  (unless (or output-file control-uri-supplied?)
    (missing-required-initarg 'record :output-file-or-control-uri)))

(defmethod shared-initialize :after
    ((instance   record)
     (slot-names t)
     &key
     (channel-allocation-spec nil channel-allocation-spec-supplied?)
     (flush-strategy-spec     nil flush-strategy-spec-supplied?))
  (when channel-allocation-spec-supplied?
    (setf (record-%channel-allocation instance)
          (rsbag.rsb:make-channel-strategy
           (parse-instantiation-spec channel-allocation-spec))))
  (when flush-strategy-spec-supplied?
    (setf (record-%flush-strategy instance)
          (parse-instantiation-spec flush-strategy-spec))))

(defun call-with-control-service (uri connection make-connection loop)
  "Expose an RPC server at URI that allows remote clients to control
   CONNECTION while LOOP executes."
  (log:info "~@<Exposing control interface at URI ~A~@:>" uri)
  (let+ ((connection connection)
         (running?   nil)
         (close?     nil)
         (exit?      nil)
         (lock       (bt:make-lock))
         (condition  (bt:make-condition-variable :name "Connection"))
         ((&flet wait-for-connection (&optional thunk)
            (bt:with-lock-held (lock)
              (loop :until (or exit? connection) :do
                 (bt:condition-wait condition lock)))
            (when (and connection thunk) (funcall thunk))))
         ((&flet wait-for-close (&optional thunk)
            (bt:with-lock-held (lock)
              (loop :until (or close? exit?) :do
                 (bt:condition-wait condition lock))
              (when thunk (funcall thunk)))))
         ((&flet check-connection (context)
            (unless connection
              (error "~@<Cannot ~A without open bag.~@:>" context))))
         ((&flet close-connection ()
            (log:info "~@<Closing ~A~@:>" connection)
            (setf close? t)
            (bt:condition-notify condition))))
    (rsb:with-participant (server :local-server uri)
      (rsb.patterns.request-reply:with-methods (server)
          (;; Connection level
           ("isstarted" ()
             (bt:with-lock-held (lock)
               (and connection running?)))
           ("start" ()
             (bt:with-lock-held (lock)
               (check-connection "start recording")
               (when running?
                 (error "~@<~A is already recording.~@:>" connection))
               (log:info "~@<Starting recording with ~A~@:>" connection)
               (rsbag.rsb:start connection)
               (setf running? t))
             (values))
           ("stop" ()
             (bt:with-lock-held (lock)
               (check-connection "stop recording")
               (unless running?
                 (error "~@<~A is not recording.~@:>" connection))
               (log:info "~@<Stopping recording with ~A~@:>" connection)
               (rsbag.rsb:stop connection)
               (setf running? nil))
             (values))

           ;; Bag level
           ("isopen" ()
             (bt:with-lock-held (lock)
               (when connection
                 (namestring
                  (bag-location (rsbag.rsb:connection-bag connection))))))
           ("open" (filename string)
             (bt:with-lock-held (lock)
               (when connection
                 (error "~@<Recording ~A in progress, cannot open a ~
                         different bag.~@:>"
                        connection))
               (log:info "~@<Opening ~S~@:>" filename)
               (setf connection (funcall make-connection filename)
                     running?   nil
                     close?     nil)
               (bt:condition-notify condition))
             (values))
           ("close" ()
             (bt:with-lock-held (lock)
               (check-connection "close bag")
               (close-connection))
             (bt:with-lock-held (lock)
               (loop :while connection :do
                  (bt:condition-wait condition lock)))
             (values))

           ;; Process level
           ("terminate" ()
             (log:info "~@<Terminating~@:>")
             (bt:with-lock-held (lock)
               (setf exit? t)
               (bt:condition-notify condition))
             (values)))

        ;; Send ready event for clients to wait on.
        (let+ (((&accessors (path puri:uri-path)) uri))
          (unless (ends-with #\/ path)
            (setf path (concatenate 'string path "/")))
          (rsb:with-participant
              (informer :informer (puri:merge-uris "state/ready" uri))
            (rsb:send informer rsb.converter:+no-value+)))

        (loop :until exit? :do
           (wait-for-connection
            (lambda () (funcall loop connection #'wait-for-close)))
           (bt:with-lock-held (lock)
             (if close?
                 (progn
                   (setf connection nil)
                   (bt:condition-notify condition))
                 (setf exit? t))))))))

(defun progress-style/channels (stream connection)
  (format stream "~A ~@<~@;~{~A~^, ~}~@:>~%"
          (local-time:now)
          (bag-channels (rsbag.rsb:connection-bag connection))))

(defun progress-style/entries (stream connection)
  (format stream "~A ~:D entr~:@P~%"
          (local-time:now)
          (reduce #'+ (bag-channels (rsbag.rsb:connection-bag connection))
                  :key #'length)))

(defmethod rsb.tools.commands:command-execute ((command record)
                                               &key error-policy)
  (let+ (((&accessors-r/o (uris               rsb.tools.commands:command-uris)
                          (filters            record-filters)
                          (output-file        command-output-file)
                          (force?             command-force?)
                          (index-timestamp    command-index-timestamp)
                          (channel-allocation record-channel-allocation)
                          (flush-strategy     record-flush-strategy)
                          (control-uri        record-control-uri)
                          (progress-style     command-progress-style))
          command)
         ((&flet make-connection (filename)
            (apply #'rsbag.rsb:events->bag uris filename
                   :error-policy     error-policy
                   :timestamp        index-timestamp
                   :channel-strategy channel-allocation
                   :filters          filters
                   :start?           (not control-uri)
                   :if-exists        (if force? :supersede :error)
                   (when flush-strategy
                     (list :flush-strategy flush-strategy)))))
         ((&flet recording-loop (connection wait-thunk)
            (rsbag.rsb:with-open-connection (connection connection)
              (with-interactive-interrupt-exit () ; TODO does not belong here
                (funcall wait-thunk)))))
         (progress-style (when progress-style
                           (curry (ecase progress-style
                                    (:entries  #'progress-style/entries)
                                    (:channels #'progress-style/channels))
                                  *info-output*))))
    (if control-uri
        (call-with-control-service
         control-uri (when output-file (make-connection output-file))
         #'make-connection #'recording-loop)
        (let ((connection (make-connection output-file)))
          (recording-loop connection
                          (lambda ()
                            (iter (sleep 10)
                                  (when progress-style
                                    (funcall progress-style connection)))))))))
