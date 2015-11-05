;;;; main.lisp --- Main function of the bag-merge program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.merge)

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (format nil "Copy entries from input files matching GLOB-EXPRESSION ~
               or from an explicitly given list of INPUT-FILEs into ~
               the specified output file. In addition to the canonical ~
               globbing syntax, expressions of the form~@
               ~@
               ~2@TSOMESTUFF/**/MORESTUFF~@
               ~@
               can be used to search directories recursively.~@
               ~@
               The file formats of input files and the output file are ~
               determined based on the file ~
               type (extension). Currently, the following file formats ~
               are supported:~{~&+ ~4A (extension: \".~(~:*~A~)\")~}.~@
               "
          (mapcar #'car (rsbag.backend:backend-classes))))

(defun make-example-string (&key (program-name "bag merge"))
  "Make and return a string containing usage examples of the program."
  (format nil "~2@T~A -o bla.tide '/vol/my-separate-logs/*.tide'~@
               ~@
               Merge all log files matching the glob expression ~
               \"/vol/my-separate-logs/*.tide\" into a single log file ~
               named \"bla.tide\". Note the quotes which prevent the ~
               shell from interpreting the glob expression.~@
               ~@
               "
          program-name))

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "bag merge"))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "GLOB-EXPRESSION | INPUT-FILE+"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Transform Options")
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
              (flag    :long-name     "force"
                       :description
                       "Should the output file be overwritten in case it already exists?")
              (stropt  :long-name     "channel"
                       :short-name    "c"
                       :argument-name "NAME-OR-REGEXP"
                       :description
                       "Select the channels matching NAME-OR-REGEXP for merging. This option can be specified multiple times.")
              (stropt  :long-name     "transform-datum"
                       #+todo :argument-name
                       #+todo :description #+todo "TODO")
              (stropt  :long-name     "transform-timestamp"
                       :argument-name "TIMESTAMP-NAME"
                       :description
                       "Index events by TIMESTAMP-NAME instead of the create timestamp.")
              (enum    :long-name     "show-progress"
                       :short-name    "p"
                       :enum          '(:none :line)
                       :default-value :line
                       :argument-name "STYLE"
                       :description
                       "Indicate progress of the ongoing playback using style STYLE."))
   ;; Append IDL options.
   :item    (make-idl-options)
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-example-string
                                    :program-name program-name)))))

(defun make-channel-filter (specs)
  (when specs
    (apply #'disjoin
           (mapcar (lambda (spec)
                     (lambda (channel)
                       (cl-ppcre:scan spec (channel-name channel))))
                   specs))))

(defun main (program-pathname args)
  "Entry point function of the bag-merge program."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " merge")))
    (update-synopsis :program-name program-name)
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsbag-tools-merge-system:version/list :commit? t)
     :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
                            :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main))))

  (let ((error-policy        (maybe-relay-to-thread
                              (process-error-handling-options)))
        (input-files         (remainder))
        (output-files        (or (getopt :long-name "output-file")
                                 (error "~@<No output file specified.~@:>")))
        (force?              (getopt :long-name "force"))
        (channels            (let ((specs (iter (for channel next (getopt :long-name "channel"))
                                                (while channel)
                                                (collect channel))))
                               (or (make-channel-filter specs) t)))
        (progress-style      (getopt :long-name "show-progress"))
        (transform/datum     (getopt :long-name "transform-datum"))
        (transform/timestamp (getopt :long-name "transform-timestamp")))
    (rsb.common:with-error-policy (error-policy)
      (rsb.formatting:with-print-limits (*standard-output*)
        (with-logged-warnings
          ;; Load IDLs as specified on the commandline.
          (process-idl-options :purpose '(:packed-size :serializer :deserializer))

          (let* ((transform/datum
                   (when transform/datum
                     (eval (read-from-string transform/datum))))
                 (transform/timestamp
                   (when transform/timestamp
                     (read-from-string transform/timestamp)))
                 (command
                   (apply #'rsb.tools.commands:make-command
                          :transform
                          :service        'rsbag.tools.commands::command
                          :input-files    input-files
                          :output-file    output-files
                          :force?         force?
                          :channels       channels
                          :progress-style progress-style
                          (append
                           (when transform/datum
                             (list :transform/datum transform/datum))
                           (etypecase transform/timestamp
                             (null
                              '())
                             (keyword
                              (list :transform/timestamp
                                    (lambda (timestamp datum)
                                      (declare (ignore timestamp))
                                      (rsb:timestamp datum transform/timestamp)))))))))

            (rsb.tools.commands:command-execute
             command :error-policy error-policy)))))))
