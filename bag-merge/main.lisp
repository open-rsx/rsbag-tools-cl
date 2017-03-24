;;;; main.lisp --- Main function of the bag-merge program.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.merge)

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (rsbag.tools.commands::augment-documentation-with-backends
   "Copy entries from input files matching GLOB-EXPRESSION or from an ~
    explicitly given list of INPUT-FILEs into the specified output ~
    file. In addition to the canonical globbing syntax, expressions of ~
    the form~@
    ~@
    ~2@TSOMESTUFF/**/MORESTUFF~@
    ~@
    can be used to search directories recursively.~@
    ~@
    The file formats of input files and the output file are determined ~
    based on the file type (extension)."))

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
   :item    (make-replay-options
             :show                    show
             :replay-strategy-default "as-fast-as-possible"
             :show-progress-default   :line
             :action                  "processing")
   :item    (defgroup (:header "Transform Options")
              (path    :long-name     "output-file"
                       :short-name    "o"
                       :type          :file
                       :description
                       (rsbag.tools.commands::augment-documentation-with-backends
                        "Name of the file into which captured events ~
                         should be written. The file format is ~
                         determined based on the file ~
                         type (extension)."))
              (flag    :long-name     "force"
                       :description
                       "Should the output file be overwritten in case it already exists?")
              (make-index-timestamp-option)
              (make-channel-allocation-option :show show)
              (stropt  :long-name     "transform-datum"
                       #+todo :argument-name
                       #+todo :description #+todo "TODO")
              (stropt  :long-name     "transform-timestamp"
                       :argument-name "TIMESTAMP-NAME"
                       :description
                       "Index events by TIMESTAMP-NAME instead of the create timestamp."))
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
                       'string (namestring program-pathname) " merge"))
        (*readtable* (copy-readtable *readtable*)))
    (update-synopsis :program-name program-name)
    (local-time:enable-read-macros)
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsbag-tools-merge-system:version/list :commit? t)
     :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
                            :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main))))

  (let+ ((error-policy        (maybe-relay-to-thread
                               (process-error-handling-options)))
         (input-files         (remainder))
         (channels            (let ((specs (iter (for channel next (getopt :long-name "channel"))
                                                 (while channel)
                                                 (collect channel))))
                                (or (make-channel-filter specs) t)))
         ((&values start-time start-index end-time end-index)
          (process-bounds-options))
         (filters             (iter (for spec next (getopt :long-name "filter"))
                                    (while spec)
                                    (collect (apply #'rsb.filter:filter
                                                    (parse-instantiation-spec spec)))))
         (progress-style      (getopt :long-name "show-progress"))
         (transform/datum     (getopt :long-name "transform-datum"))
         (transform/timestamp (getopt :long-name "transform-timestamp"))
         (output-file         (or (getopt :long-name "output-file")
                                  (error "~@<No output file specified.~@:>")))
         (force?              (getopt :long-name "force"))
         (index-timestamp     (when-let ((value (getopt :long-name "index-timestamp")))
                                (make-keyword value)))
         (channel-allocation  (getopt :long-name "channel-allocation")))
    (rsb.formatting:with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)
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
                         :channels       channels
                         :start-time     start-time
                         :start-index    start-index
                         :end-time       end-time
                         :end-index      end-index
                         :filters        filters
                         :output-file    output-file
                         :force?         force?
                         :progress-style progress-style
                         (append
                          (when index-timestamp
                            (list :index-timestamp index-timestamp))
                          (when channel-allocation
                            (list :channel-allocation-spec channel-allocation))
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
