;;;; main.lisp --- Main function of the bag-play program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.play)

(defun update-synopsis (&key
                        (show         :default)
                        (program-name "rsbag play"))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "INPUT-FILE [BASE-URI]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (make-replay-options :show show :action "replay")
   ;; Append RSB options.
   :item    (make-options
             :show? (or (eq show t)
                        (and (listp show) (member :rsb show))))
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
  "Entry point function of the bag-play program."
  (let ((program-name (concatenate
                       'string (namestring program-pathname) " play"))
        (*readtable* (copy-readtable *readtable*))) ; TODO still necessary?
    (update-synopsis :program-name program-name)
    (setf *configuration* (options-from-default-sources))
    (local-time:enable-read-macros)
    (process-commandline-options
     :commandline     (list* program-name args)
     :version         (cl-rsbag-tools-play-system:version/list :commit? t)
     :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
                            :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
     :update-synopsis (curry #'update-synopsis :program-name program-name)
     :return          (lambda () (return-from main))))

  (unless (<= 1 (length (remainder)) 2)
    (error "~@<Specify input file and, optionally, base URI.~@:>"))

  (let+ ((error-policy (maybe-relay-to-thread
                        (process-error-handling-options)))
         ((input-file &optional (base-uri "/")) (remainder))
         (channels (let ((specs (iter (for channel next (getopt :long-name "channel"))
                                      (while channel)
                                      (collect channel))))
                     (or (make-channel-filter specs) t)))
         ((&values start-time start-index end-time end-index)
          (process-bounds-options))
         (loop            (getopt :long-name "loop"))
         (replay-strategy (getopt :long-name "replay-strategy"))
         (progress-style  (getopt :long-name "show-progress")))
    (rsb.formatting:with-print-limits (*standard-output*)
      (with-logged-warnings
        (rsb.common:with-error-policy (error-policy)
          (let ((command
                 (rsb.tools.commands:make-command
                  :play
                  :service             'rsbag.tools.commands::command
                  :input-files          (list input-file)
                  :channels             channels
                  :start-time           start-time
                  :start-index          start-index
                  :end-time             end-time
                  :end-index            end-index
                  :num-repetitions      loop
                  :replay-strategy-spec replay-strategy
                  :destination          base-uri
                  :progress-style       progress-style)))

            (with-interactive-interrupt-exit ()
              (rsb.tools.commands:command-execute
               command :error-policy error-policy))

            (when (eq progress-style :line)
              (terpri *standard-output*))))))))
