;;;; main.lisp --- Main function of the bag-cat program.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.cat)

;;; Help and main functions

(defun make-help-string ()
  "Return a help that explains the commandline option interface."

  (rsbag.tools.commands::augment-documentation-with-backends
   "Outputs data from channels in the bag file INPUT-FILE-OR-- (or ~
    standard input, if \"-\" is specified) on standard output.~@
    ~@
    The file format of INPUT-FILE is guessed based on the filename."))

(defun make-examples-string (&key (program-name "bag cat"))
  "Make and return a string containing usage examples of the program."
  (format nil
          "~2T~A /tmp/everything.tide~@
           ~@
           Output the data from all channels in the log file ~
           \"/tmp/everything.tide\" ordered by timestamps.~@
           ~@
           ~2T~:*~A -c isr /tmp/nao.tide~@
           ~@
           Output the data from all channels of the log file ~
           \"/tmp/nao.tide\" the names of which contain the string ~
           \"isr\".~@
           ~@
           ~2T~:*~A --channel 'STRING$' --channel 'BYTES$' log.tide~@
           ~@
           Output the data from all channels of the log file ~
           \"log.tide\" whose names end in either \"STRING\" or ~
           \"BYTES\".~@
           ~@
           ~2T~:*~A --style 'payload :separator (#\\Newline (:rule ~
           #\\-))' log.tide ~@
           ~@
           Print event payloads separating payloads of different ~
           events by newlines and horizontal rules.~@
           ~@
           ~2T~:*~A --style 'programmable/template :template ~
           #P\"my-template-file.template\"' log.tide~@
           ~@

           Format events in the log file \"log.tide\" by applying the ~
           template in \"my-template-file.template\" to each ~
           event. See output of --help-for styles for more ~
           information."
          program-name))

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "bag cat"))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "INPUT-FILE-OR--"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (make-replay-options
             :show                    show
             :replay-strategy-default "as-fast-as-possible"
             :action                  "processing")
   :item    (defgroup (:header "Output Options")
              (stropt  :long-name     "style"
                       :default-value "payload"
                       :argument-name "SPEC"
                       :description
                       (make-style-help-string :show show))
              (enum    :long-name     "target-stream"
                       :enum          '(:stdout :standard-output
                                        :stderr :error-output)
                       :default-value :standard-output
                       :argument-name "STREAM-NAME"
                       :description
                       "Stream to which produced output should be sent."))
   ;; Append IDL options.
   :item    (make-idl-options)
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string
                                    :program-name program-name)))))

(defun make-channel-filter (specs)
  (when specs
    (apply #'disjoin
           (mapcar (lambda (spec)
                     (lambda (channel)
                       (cl-ppcre:scan spec (channel-name channel))))
                   specs))))

(defun main (program-pathname args)
  "Entry point function of the bag-cat program."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " cat"))
        (*readtable* (copy-readtable *readtable*))) ; TODO still necessary?
    (update-synopsis :program-name program-name)
    (local-time:enable-read-macros)
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsbag-tools-cat-system:version/list :commit? t)
     :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
                            :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main))))

  (when (emptyp (remainder))
    (error "~@<Specify at least one input file.~@:>"))

  (let+ ((error-policy (maybe-relay-to-thread
                        (process-error-handling-options)))
         (input-files  (remainder))
         (channels     (let ((specs (iter (for channel next (getopt :long-name "channel"))
                                          (while channel)
                                          (collect channel))))
                         (or (make-channel-filter specs) t)))
         ((&values start-time start-index end-time end-index)
          (process-bounds-options))
         (loop            (getopt :long-name "loop"))
         (replay-strategy (getopt :long-name "replay-strategy"))
         (style           (getopt :long-name "style"))
         (target-stream   (getopt :long-name "target-stream")))
    (with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)
          ;; Load IDLs as specified on the commandline.
          (process-idl-options)

          ;; Gather the following things from commandline options:
          ;; + input file(s)
          ;; + selected channels for replay
          ;; + temporal/index range for replay
          ;; + formatting style
          ;; Pass all of these to `bag->events' for and start the
          ;; resulting connection.
          (let ((command
                 (rsb.tools.commands:make-command
                  :cat
                  :service              'rsbag.tools.commands::command
                  :input-files          input-files
                  :channels             channels
                  :start-time           start-time
                  :start-index          start-index
                  :end-time             end-time
                  :end-index            end-index
                  :num-repetitions      loop
                  :replay-strategy-spec replay-strategy
                  :style-spec           style
                  :stream-spec          target-stream)))
            (with-interactive-interrupt-exit ()
              (rsb.tools.commands:command-execute
               command :error-policy error-policy))))))))
