;;;; main.lisp --- Main function of the bag-introspect program.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.introspect)

;;; Help and main functions

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (format nil
          "Gather introspection information from the log file ~
           INPUT-FILE-OR-- (or standard input, if \"-\" is specified) ~
           and display it on standard output (or otherwise process it).~@
           ~@
           The file format of INPUT-FILE is guessed based on the ~
           filename. Currently, the following file formats are ~
           supported:~{~&+ ~4A (extension: \".~(~:*~A~)\")~}"
          (mapcar #'car (rsbag.backend:backend-classes))))

(defun make-examples-string (&key (program-name "bag introspect"))
  "Make and return a string containing usage examples of the program."
  (format nil
          "~2T~A log-file.tide~@
           ~@
           Gather introspection information from the log file ~
           log-file.tide and display it using the default formatting ~
           style."
          program-name))

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "bag introspect"))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "INPUT-FILE-OR--"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (make-replay-options :show             show
                                 :action           "introspection"
                                 :channel?         nil
                                 :loop?            nil
                                 :replay-strategy? nil)
   :item    (defgroup (:header "Output Options")
              (stropt  :long-name     "style"
                       :default-value "object-tree"
                       :argument-name "SPEC"
                       :description
                       (with-output-to-string (stream)
                         (rsb.common:with-abbreviation (stream :styles show)
                           (write-string (rsb.formatting::make-style-service-help-string
                                          :service           'rsb.formatting.introspection::style
                                          :initarg-blacklist '(:database))
                                         stream))))
              (enum    :long-name     "target-stream"
                       :enum          '(:stdout :standard-output
                                        :stderr :error-output)
                       :default-value :standard-output
                       :argument-name "STREAM-NAME"
                       :description
                       "Stream to which produced output should be sent."))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string
                                    :program-name program-name)))))

(defun main (program-pathname args)
  "Entry point function of the bag-introspect program."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " introspect"))
        (*readtable* (copy-readtable *readtable*))) ; TODO still necessary?
    (update-synopsis :program-name program-name)
    (local-time:enable-read-macros)
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsbag-tools-introspect-system:version/list :commit? t)
     :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
                            :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main))))

  (when (emptyp (remainder))
    (error "~@<Specify at least one input file.~@:>"))

  (let+ ((error-policy (maybe-relay-to-thread
                        (process-error-handling-options)))
         (input-files  (remainder))
         ((&values start-time start-index end-time end-index)
          (process-bounds-options))
         (style           (getopt :long-name "style"))
         (target-stream   (getopt :long-name "target-stream"))
         (progress-style  (getopt :long-name "show-progress")))
    (rsb.formatting:with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)
          ;; Gather the following things from commandline options:
          ;; + input file(s)
          ;; + temporal/index range for replay
          ;; + formatting style
          ;; Pass all of these to `bag->events' for and start the
          ;; resulting connection.
          (let ((command
                 (rsb.tools.commands:make-command
                  :introspect
                  :service        'rsbag.tools.commands::command
                  :input-files    input-files
                  :start-time     start-time
                  :start-index    start-index
                  :end-time       end-time
                  :end-index      end-index
                  :style-spec     style
                  :stream-spec    target-stream
                  :progress-style progress-style)))
            (with-interactive-interrupt-exit ()
              (rsb.tools.commands:command-execute
               command :error-policy error-policy))))))))
