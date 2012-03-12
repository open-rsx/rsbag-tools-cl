;;; options.lisp --- Common functions related to commandline options.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsbag.common)

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
	     :typespec      '(or real local-time:timestamp)
	     :argument-name "TIMESTAMP-OR-SECONDS"
	     :description
	     "Start replaying events at the point in time indicated by TIMESTAMP-OR-SECONDS. When the value should be parsed as a timestamp, the syntax @[YYYY-MM-DDT]HH:MM:SS has to be used. A single real number is interpreted as time in seconds relative to the beginning of the replay. Mutually exclusive with --start-index.")
    (lispobj :long-name     "start-index"
	     :short-name    "S"
	     :typespec      'non-negative-integer
	     :argument-name "INDEX"
	     :description
	     "Index of the event at which the replay should
start. Mutually exclusive with --start-time.")
    (lispobj :long-name     "end-time"
	     :short-name    "e"
	     :typespec      '(or real local-time:timestamp)
	     :argument-name "TIMESTAMP-OR-SECONDS"
	     :description
	     "Stop replaying events at the point in time indicated by TIMESTAMP-OR-SECONDS. When the value should be parsed as a timestamp, the syntax @[YYYY-MM-DDT]HH:MM:SS has to be used. A single real number is interpreted as time in seconds relative to the beginning of the replay. Mutually exclusive with --end-index.")
    (lispobj :long-name     "end-index"
	     :short-name    "E"
	     :typespec      'non-negative-integer
	     :argument-name "INDEX"
	     :description
	     "Index of the event at which the replay should
end. Mutually exclusive with --end-time.")
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
2. start-index: `non-negative-integer' or nil
3. end-time:    `local-time:timestamp' or nil
4. end-index:   `non-negative-integer' or nil"
  (bind (((start-time start-index end-time end-index)
	  (map 'list (curry #'getopt :long-name)
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
