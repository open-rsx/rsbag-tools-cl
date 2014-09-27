;;;; main.lisp --- Main function of the bag-record program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.record)

(defun invoke-with-control-service (uri connection make-connection loop)
  "Expose an RPC server at URI that allows remote clients to control
CONNECTION while THUNK executes."
  (log:info "~@<Exposing control interface at URI ~A~@:>" uri)
  (let+ ((connection connection)
         (running?   nil)
         (close?     nil)
         (exit?      nil)
         (lock       (bt:make-lock))
         (condition  (bt:make-condition-variable :name "Connection"))
         ((&flet wait-for-connection (&optional thunk)
           (bt:with-lock-held (lock)
             (loop :until (or exit? connection) :do (bt:condition-wait condition lock)))
           (when (and connection thunk) (funcall thunk))))
         ((&flet wait-for-close (&optional thunk)
           (bt:with-lock-held (lock)
             (loop :until (or close? exit?) :do (bt:condition-wait condition lock))
             (when thunk (funcall thunk)))))
         ((&flet check-connection (context)
            (unless connection
              (error "~@<Cannot ~A without open bag.~@:>" context))))
         ((&flet close-connection ()
            (log:info "~@<Closing ~A~@:>" connection)
            (setf close? t)
            (bt:condition-notify condition))))
    (with-local-server (server uri)
      (with-methods (server)
        (;; Connection level
         ("start" ()
           (bt:with-lock-held (lock)
             (check-connection "start recording")
             (when running?
               (error "~@<~A is already recording.~@:>" connection))
             (log:info "~@<Starting recording with ~A~@:>" connection)
             (start connection)
             (setf running? t))
           (values))
         ("stop" ()
           (bt:with-lock-held (lock)
             (check-connection "stop recording")
             (unless running?
               (error "~@<~A is not recording.~@:>" connection))
             (log:info "~@<Stopping recording with ~A~@:>" connection)
             (stop connection)
             (setf running? nil))
           (values))

         ;; Bag level
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
             (loop :while connection :do (bt:condition-wait condition lock)))
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
          (with-informer (informer (puri:merge-uris "state/ready" uri) t)
            (send informer rsb.converter:+no-value+)))

        (loop :until exit? :do
           (wait-for-connection
            (lambda () (funcall loop connection #'wait-for-close)))
           (bt:with-lock-held (lock)
             (if close?
                 (progn
                   (setf connection nil)
                   (bt:condition-notify condition))
                 (setf exit? t))))))))

(defun update-synopsis (&key
                        (show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "[URIS]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Recording Options")
              (path    :long-name     "output-file"
                       :short-name    "o"
                       :type          :file
                       :description
                       (format nil "Name of the file into which ~
                                    captured events should be ~
                                    written. The file format is ~
                                    determined based on the file ~
                                    type (extension). Currently, the ~
                                    following file formats are ~
                                    supported:~{~&+ ~4A (extension: ~
                                    \".~(~:*~A~)\")~}."
                               (mapcar #'car (rsbag.backend:backend-classes))))
              (switch  :long-name     "force"
                       :default-value nil
                       :description
                       "Should the output file be overwritten in case it already exists?")
              (stropt  :long-name     "index-timestamp"
                       :default-value "SEND"
                       :argument-name "NAME"
                       :description
                       "Name of the timestamp which should be used to index events in the created log file.")
              (stropt  :long-name     "channel-allocation"
                       :short-name    "a"
                       :default-value "scope-and-type"
                       :argument-name "SPEC"
                       :description
                       (make-channel-strategy-help-string :show show))
              (stropt  :long-name     "filter"
                       :short-name    "f"
                       :argument-name "SPEC"
                       :description
                       (make-filter-help-string :show show))
              (stropt  :long-name     "flush-strategy"
                       :default-value "property-limit :property :length/bytes :limit 33554432"
                       :argument-name "SPEC"
                       :description
                       (make-flush-strategy-help-string :show show))
              (stropt  :long-name     "control-uri"
                       :short-name    "c"
                       :argument-name "URI"
                       :description
                       "A URI specifying the root scope and transport configuration of an RPC server exposing methods which allow controlling the recording process. Currently, the following methods are provided:

start : void -> void

  Restart recording after it has been stopped. Only applicable if a bag has been opened.

stop : void -> void

  Stop recording allowing it to be restarted later. Only applicable if a bag has been opened.

open : string -> void

  Open the specified file to record into it. Does not start recording. Only applicable if not bag is currently open.

close : void -> void

  Close the current bag. Only applicable if a bag is open.

terminate : void -> void

  Terminate the recording process and the program."))
   ;; Append RSB options.
   :item    (make-options
             :show? (or (eq show t)
                        (and (listp show) (member :rsb show))))
   ;; Append IDL options.
   :item    (make-idl-options)
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string)))))

(defun main ()
  "Entry point function of the bag-record program."
  (update-synopsis)
  (setf *configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsbag-tools-record-system:version/list :commit? t)
   :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
                          :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
   :update-synopsis #'update-synopsis
   :return          (lambda () (return-from main)))

  (unless (remainder)
    (error "~@<Specify at least one URI from which events should be ~
            recorded.~@:>"))

  (with-logged-warnings
    ;; Load IDLs as specified on the commandline.
    (process-idl-options)

    (rsb.formatting:with-print-limits (*standard-output*)
        ;; Create a reader and start the receiving and printing loop.
        (let+ ((error-policy    (maybe-relay-to-thread
                                 (process-error-handling-options)))
               (control-uri     (when-let ((string (getopt :long-name "control-uri")))
                                (puri:parse-uri string)))
               (uris            (mapcar #'puri:parse-uri (remainder)))
               (output/pathname (or (getopt :long-name "output-file")
                                    (unless control-uri
                                      (error "~@<No output file specified.~@:>"))))
               (force           (getopt :long-name "force"))
               (timestamp       (make-keyword
                                 (getopt :long-name "index-timestamp")))
               (channel-alloc   (parse-instantiation-spec
                                 (getopt :long-name "channel-allocation")))
               (filters         (iter (for spec next (getopt :long-name "filter"))
                                      (while spec)
                                      (collect (apply #'rsb.filter:filter
                                                      (parse-instantiation-spec spec)))))
               (flush-strategy  (parse-instantiation-spec
                                 (getopt :long-name "flush-strategy")))
               ((&flet make-connection (filename)
                  (events->bag uris filename
                               :error-policy     error-policy
                               :timestamp        timestamp
                               :channel-strategy channel-alloc
                               :filters          filters
                               :flush-strategy   flush-strategy
                               :start?           (not control-uri)
                               :if-exists        (if force :supersede :error))))
               ((&flet recording-loop (connection wait-thunk)
                  (with-open-connection (connection connection)
                    (with-interactive-interrupt-exit ()
                      (funcall wait-thunk))))))

          (log:info "~@<Using URIs ~@<~@;~{~A~^, ~}~@:>~@:>" uris)
          (with-error-policy (error-policy)
            (if control-uri
                (invoke-with-control-service
                 control-uri (when output/pathname
                               (make-connection output/pathname))
                 #'make-connection #'recording-loop)
                (let ((connection (make-connection output/pathname)))
                  (recording-loop connection
                                  (lambda ()
                                    (iter (sleep 10)
                                          (format t "~A ~@<~@;~{~A~^, ~}~@:>~%"
                                                  (local-time:now)
                                                  (bag-channels (connection-bag connection)))))))))))))
