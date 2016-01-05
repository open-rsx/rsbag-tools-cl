;;;; main.lisp --- Main function of the bag-record program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.record)

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "rsbag record"))
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
                       :default-value "collapse-reserved :next :scope-and-type"
                       :argument-name "SPEC"
                       :description
                       (make-channel-strategy-help-string :show show))
              (stropt  :long-name     "filter"
                       :short-name    "f"
                       :argument-name "SPEC"
                       :description
                       (make-filter-help-string :show show))
              (stropt  :long-name     "flush-strategy"
                       :argument-name "SPEC"
                       :description
                       (make-flush-strategy-help-string :show show))
              (stropt  :long-name     "control-uri"
                       :short-name    "c"
                       :argument-name "URI"
                       :description
                       "A URI specifying the root scope and transport configuration of an RPC server exposing methods which allow controlling the recording process. Currently, the following methods are provided:

isstarted : void -> bool

  Return true if a log file has been opened for recording and recording is currently in progress and false otherwise.

start : void -> void

  Restart recording after it has been stopped. Only applicable if a bag has been opened.

stop : void -> void

  Stop recording allowing it to be restarted later. Only applicable if a bag has been opened.

isopen : void -> string or false

  If a log file has been opened for recording, return its path as a string. Otherwise return false.

open : string -> void

  Open the specified file to record into it. Does not start recording. Only applicable if not bag is currently open.

close : void -> void

  Close the current bag. Only applicable if a bag is open.

terminate : void -> void

  Terminate the recording process and the program.")
              (switch  :long-name     "introspection-survey"
                       :default-value t
                       :description
                       (format nil "Perform an introspection survey at ~
                                    the beginning of the recording to ~
                                    ensure it contains a complete ~
                                    snapshot of the system structure ~
                                    from which incremental changes can ~
                                    be tracked."))
              (enum    :long-name     "progress-style"
                       :short-name    "p"
                       :enum          '(:none :entries :channels)
                       :default-value :entries
                       :argument-name "SPEC"
                       :description
                       (format nil "The style used to display ~
                                    information about the recorded ~
                                    data. \"none\" to disable.")))
   ;; Append RSB options.
   :item    (make-options
             :show? (or (eq show t)
                        (and (listp show) (member :rsb show))))
   ;; Append IDL options.
   :item    (make-idl-options)
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string
                                    :program-name program-name)))))

(defun main (program-pathname args)
  "Entry point function of the bag-record program."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " record") ))
    (update-synopsis :program-name program-name)
    (setf *configuration* (options-from-default-sources))
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsbag-tools-record-system:version/list :commit? t)
     :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
                            :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main))))

  (unless (remainder)
    (error "~@<Specify at least one URI from which events should be ~
            recorded.~@:>"))

  (let ((error-policy          (maybe-relay-to-thread
                                (process-error-handling-options)))
        (uris                  (remainder))
        (filters               (iter (for spec next (getopt :long-name "filter"))
                                     (while spec)
                                     (collect (apply #'rsb.filter:filter
                                                     (parse-instantiation-spec spec)))))
        (output-file           (getopt :long-name "output-file"))
        (force?                (getopt :long-name "force"))
        (index-timestamp       (make-keyword
                                (getopt :long-name "index-timestamp")))
        (channel-allocation    (getopt :long-name "channel-allocation"))
        (flush-strategy        (getopt :long-name "flush-strategy"))
        (control-uri           (when-let ((string (getopt :long-name "control-uri")))
                                 (puri:parse-uri string)))
        (introspection-survey? (getopt :long-name "introspection-survey"))
        (progress-style        (getopt :long-name "progress-style")))
    (rsb.formatting:with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)
          ;; Load IDLs as specified on the commandline.
          (process-idl-options)

          (let ((command
                 (apply #'rsb.tools.commands:make-command :record
                  :service                 'rsbag.tools.commands::command
                  :uris                    uris
                  :filters                 filters
                  :output-file             output-file
                  :force?                  force?
                  :index-timestamp         index-timestamp
                  :channel-allocation-spec channel-allocation
                  :control-uri             control-uri
                  :introspection-survey?   introspection-survey?
                  :progress-style          progress-style
                  (when flush-strategy
                    (list :flush-strategy-spec flush-strategy)))))
            (rsb.tools.commands:command-execute
             command :error-policy error-policy)))))))
