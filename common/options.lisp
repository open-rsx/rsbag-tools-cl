;;;; options.lisp --- Common functions related to commandline options.
;;;;
;;;; Copyright (C) 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.common)

(defun make-replay-options (&key
                            (show                    :default)
                            (replay-strategy-default "recorded-timing"))
  "Return a `clon:group' instance containing replay-related
commandline options:
+ channel
+ start-time
+ start-index
+ end-index
+ replay-strategy
+ show-progress"
  (defgroup (:header "Playback Options")
    (stropt  :long-name     "channel"
             :short-name    "c"
             :argument-name "NAME-OR-REGEXP"
             :description
             "Select the channels matching NAME-OR-REGEXP for replay. This option can be specified multiple times.")
    (lispobj :long-name     "start-time"
             :short-name    "s"
             :typespec      'range-boundary/timestamp
             :argument-name "TIMESTAMP-OR-SECONDS"
             :description
             "Start replaying entries at the point in time indicated by TIMESTAMP-OR-SECONDS.

When the value should be parsed as a timestamp, the syntax @[YYYY-MM-DDT]HH:MM:SS has to be used.

A single positive real number is interpreted as time in seconds relative to the beginning of the replay. Similarly, a single negative real number is interpreted as time in seconds relative to the end of the replay, e.g. -2.5 indicates \"2.5 seconds before the end of the replay\".

Mutually exclusive with --start-index.")
    (lispobj :long-name     "start-index"
             :short-name    "S"
             :typespec      'integer
             :argument-name "INDEX"
             :description
             "Index of the entry at which the replay should start.

A non-negative integer N is interpreted as the (N+1)-th entry relative to the beginning of the replay, i.e. 0 designates the first entry. A negative integer N is interpreted as |N| entries back from the end of the replay.

Mutually exclusive with --start-time.")
    (lispobj :long-name     "end-time"
             :short-name    "e"
             :typespec      'range-boundary/timestamp
             :argument-name "TIMESTAMP-OR-SECONDS"
             :description
             "Stop replaying entries at the point in time indicated by TIMESTAMP-OR-SECONDS.

When the value should be parsed as a timestamp, the syntax @[YYYY-MM-DDT]HH:MM:SS has to be used.

A single real number is interpreted as time in seconds relative to the beginning of the replay. Similarly, a single negative real number is interpreted as time in seconds relative to the end of the replay, e.g. -2.5 indicates \"2.5 seconds before the end of the replay\".

Mutually exclusive with --end-index.")
    (lispobj :long-name     "end-index"
             :short-name    "E"
             :typespec      'integer
             :argument-name "INDEX"
             :description
             "Index of the entry at which the replay should end.

A non-negative integer N is interpreted as the (N+1)-th entry relative to the beginning of the replay, i.e. 0 designates the first entry. A negative integer N is interpreted as |N| entries back from the end of the replay.

Mutually exclusive with --end-time.")
    (lispobj :long-name     "loop"
             :short-name    "n"
             :typespec      '(or (eql t) positive-integer)
             :default-value 1
             :argument-name "NUMBER-OF-TIMES-OR-T"
             :description
             "Number of times the replay should be repeated.

The character \"t\" indicates that the replay should be repeated indefinitely.")
    (stropt  :long-name     "replay-strategy"
             :short-name    "r"
             :default-value replay-strategy-default
             :argument-name "SPEC"
             :description
             (make-replay-strategy-help-string :show show))
    (enum    :long-name     "show-progress"
             :short-name    "p"
             :enum          '(:none :line)
             :default-value :line
             :argument-name "STYLE"
             :description
             "Indicate progress of the ongoing playback using style STYLE.")))

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
