;;;; options.lisp --- Common functions related to commandline options.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.common)

(defun %make-group (header &rest items)
  (apply #'make-group :header header
         (mapcan #'list (circular-list :item)
                 (remove nil items))))

(defun make-replay-options
    (&key
     (show                    :default)
     (channel?                t)
     (loop?                   t)
     (replay-strategy?        t)
     (replay-strategy-default "recorded-timing")
     (show-progress-default   :none)
     (action                  (missing-required-argument :action)))
  "Return a `clon:group' instance containing some or all of the
   replay-related commandline options:
   + channel
   + start-{time,index}
   + end-{time,index}
   + loop
   + filter
   + replay-strategy
   + show-progress"
  (%make-group
   (format nil "~:(~a~) options" action)
   (when channel?
     (make-stropt  :long-name     "channel"
                   :short-name    "c"
                   :argument-name "NAME-OR-REGEXP"
                   :description
                   (format nil "Select the channels matching ~
                                NAME-OR-REGEXP for ~A. This option can ~
                                be supplied multiple times.~@
                                ~@
                                By default, NAME-OR-REGEXP can match ~
                                any substring of the channel name. to ~
                                match the whole name instead, ^REGEXP$ ~
                                can used.~@
                                ~@
                                Again by default, channel names are of ~
                                the form SCOPE:TYPE-NAME. To only ~
                                match the scope or type, the regular ~
                                expressions ^SCOPE-REGEXP: or ~
                                :TYPE-REGEXP$ can be used ~
                                respectively."
                           action)))
   (make-lispobj :long-name     "start-time"
                 :short-name    "s"
                 :typespec      'range-boundary/timestamp
                 :argument-name "TIMESTAMP-OR-SECONDS"
                 :description
                 (format nil "Start ~A of entries at the point in time ~
                              indicated by TIMESTAMP-OR-SECONDS.~@
                              ~@
                              When the value should be parsed as a ~
                              timestamp, the syntax ~
                              @[YYYY-MM-DDT]HH:MM:SS has to be ~
                              used.~@
                              ~@
                              A single positive real number is ~
                              interpreted as time in seconds relative ~
                              to the beginning of the log ~
                              file. Similarly, a single negative real ~
                              number is interpreted as time in seconds ~
                              relative to the end of the log file, ~
                              e.g. -2.5 indicates \"2.5 seconds before ~
                              the end of the log file\".~@
                              ~@
                              Mutually exclusive with --start-index."
                         action))
   (make-lispobj :long-name     "start-index"
                 :short-name    "S"
                 :typespec      'integer
                 :argument-name "INDEX"
                 :description
                 (format nil "Index of the entry at which the ~A ~
                              should start.~@
                              ~@
                              A non-negative integer N is interpreted ~
                              as the (N+1)-th entry relative to the ~
                              beginning of the log file, i.e. 0 ~
                              designates the first entry. A negative ~
                              integer N is interpreted as |N| entries ~
                              back from the end of the log file.~@
                              ~@
                              Mutually exclusive with --start-time."
                         action))
   (make-lispobj :long-name     "end-time"
                 :short-name    "e"
                 :typespec      'range-boundary/timestamp
                 :argument-name "TIMESTAMP-OR-SECONDS"
                 :description
                 (format nil "Stop ~A of entries at the point in time ~
                              indicated by TIMESTAMP-OR-SECONDS.~@
                              ~@
                              When the value should be parsed as a ~
                              timestamp, the syntax ~
                              @[YYYY-MM-DDT]HH:MM:SS has to be ~
                              used.~@
                              ~@
                              A single real number is interpreted as ~
                              time in seconds relative to the ~
                              beginning of the ~:*~A. Similarly, a ~
                              single negative real number is ~
                              interpreted as time in seconds relative ~
                              to the end of the log file, e.g. -2.5 ~
                              indicates \"2.5 seconds before the end ~
                              of the log file\".~@
                              ~@
                              Mutually exclusive with --end-index."
                         action))
   (make-lispobj :long-name     "end-index"
                 :short-name    "E"
                 :typespec      'integer
                 :argument-name "INDEX"
                 :description
                 (format nil "Index of the entry at which the ~A ~
                              should end.~@
                              ~@
                              A non-negative integer N is interpreted ~
                              as the (N+1)-th entry relative to the ~
                              beginning of the log file, i.e. 0 ~
                              designates the first entry. A negative ~
                              integer N is interpreted as |N| entries ~
                              back from the end of the log file.~@
                              ~@
                              Mutually exclusive with --end-time."
                         action))
   (when loop?
     (make-lispobj :long-name     "loop"
                   :short-name    "n"
                   :typespec      '(or (eql t) positive-integer)
                   :default-value 1
                   :argument-name "NUMBER-OF-TIMES-OR-T"
                   :description
                   (format nil "Number of times the ~A should be ~
                                repeated.~@
                                ~@
                                The character \"t\" indicates that the ~
                                ~:*~A should be repeated ~
                                indefinitely."
                           action)))
   (make-stropt :long-name     "filter"
                :short-name    "f"
                :argument-name "SPEC"
                :description
                (rsb.tools.common:make-filter-help-string :show show))
   (when replay-strategy?
     (make-stropt  :long-name     "replay-strategy"
                   :short-name    "r"
                   :default-value replay-strategy-default
                   :argument-name "SPEC"
                   :description
                   (make-replay-strategy-help-string :show show)))
   (make-enum    :long-name     "show-progress"
                 :short-name    "p"
                 :enum          '(:none :line :ready)
                 :default-value show-progress-default
                 :argument-name "STYLE"
                 :description
                 (format nil "Indicate progress of the ongoing ~A ~
                              using style STYLE."
                         action))))

(defun process-bounds-options ()
  "Retrieve values of the {start,end}-{time,index} commandline options,
   check their consistency and return them as four values:

   1. start-time:  `local-time:timestamp' or nil
   2. start-index: `integer' or nil
   3. end-time:    `local-time:timestamp' or nil
   4. end-index:   `integer' or nil"
  (let+ (((start-time start-index end-time end-index)
          (mapcar (curry #'getopt :long-name)
                  '("start-time" "start-index"
                    "end-time"   "end-index"))))
    ;; Check mutually exclusive options.
    (when (and start-time start-index)
      (error "~@<The commandline options \"start-time\" and ~
              \"start-index\" are mutually exclusive.~@:>"))
    (when (and end-time end-index)
      (error "~@<The commandline options \"end-time\" and ~
              \"end-index\" are mutually exclusive.~@:>"))

    (values start-time start-index end-time end-index)))

(defun make-index-timestamp-option (&key default)
  (apply #'make-stropt
         :long-name     "index-timestamp"
         :argument-name "NAME"
         :description   (format nil "Name of the timestamp which should ~
                                     be used to index events in the ~
                                     created log file.")
         (when default
           (list :default-value "SEND"))))

(defun make-channel-allocation-option (&key
                                       default
                                       (show :default))
  (apply #'make-stropt
         :long-name     "channel-allocation"
         :short-name    "a"
         :argument-name "SPEC"
         :description   (make-channel-strategy-help-string :show show)
         (when default
           (list :default-value default))))
