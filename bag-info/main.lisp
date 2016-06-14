;;;; main.lisp --- Main function of the bag-info program.
;;;;
;;;; Copyright (C) 2011-2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.info)

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (rsbag.tools.commands::augment-documentation-with-backends
   "Display information about BAG-FILE.~@
    ~@
    The file format of BAG-FILE is guessed based on the filename."))

(defun make-example-string (&key (program-name "bag info"))
  "Make and return a string containing usage examples of the program."
  (format nil "~2@T~A /tmp/everything.tide~@
               "
          program-name))

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "rsbag info"))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "BAG-FILE+"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Output Options")
              (flag :long-name  "compute-sizes"
                    :short-name "s"
                    :description
                    "Compute the sizes of the content of the whole log file and individual channels. This may take some time for large files.")
              (enum :long-name     "print-format"
                    :short-name    "f"
                    :enum          '(:no :short :full)
                    :default-value :short
                    :description
                    "Print format information for each channel."))
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-example-string
                                    :program-name program-name)))))

(defun main (program-pathname args)
  "Entry point function of the bag-info program."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " info")))
    (update-synopsis :program-name program-name)
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsbag-tools-info-system:version/list :commit? t)
     :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
                            :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main))))

  (when (emptyp (remainder))
    (error "~@<Specify exactly at least one input file.~@:>"))

  (let ((error-policy  (maybe-relay-to-thread
                        (process-error-handling-options)))
        (input-files   (remainder))
        (compute-sizes (getopt :long-name "compute-sizes"))
        (print-format  (getopt :long-name "print-format")))
    (with-print-limits (*standard-output*)
      (with-logged-warnings
        (with-error-policy (error-policy)
          (let ((command
                 (rsb.tools.commands:make-command
                  :info
                  :service        'rsbag.tools.commands::command
                  :input-files    input-files
                  :compute-sizes? compute-sizes
                  :print-format   print-format)))
            (rsb.tools.commands:command-execute
             command :error-policy error-policy)))))))
