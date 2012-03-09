;;; main.lisp --- Main function of the bag-play program.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(in-package :rsbag.tools.play)

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "INPUT-FILE [BASE-URI]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (defgroup (:header "Playback Options")
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
		       :default-value "recorded-timing"
		       :argument-name "SPEC"
		       :description
		       (make-replay-strategy-help-string :show show))
	      (enum    :long-name     "show-progress"
		       :short-name    "p"
		       :enum          '(:none :line)
		       :default-value :line
		       :argument-name "STYLE"
		       :description
		       "Indicate progress of the ongoing playback using style STYLE."))
   ;; Append RSB options.
   :item    (make-options
	     :show? (or (eq show t)
			(and (listp show) (member :rsb show))))
   ;; Append examples.
   :item    (defgroup (:header "Examples")
	      (make-text :contents (make-examples-string)))))

(defun make-channel-filter (specs)
  (when specs
    (apply #'disjoin
	   (map 'list #'(lambda (spec)
			  #'(lambda (channel)
			      (cl-ppcre:scan spec (channel-name channel))))
		specs))))

(defun print-progress (progress
		       index start-index end-index
		       timestamp)
  "Print the progress of the current replay onto the stream that is
the value of `*standard-output*'."
  (format *standard-output* "~C~A ~6,2,2F % ~9:D [~9:D,~9:D]"
	  #\Return
	  timestamp
	  progress
	  index start-index end-index)
  (force-output *standard-output*))

(defun process-bounds-options ()
  "Retrieve values or {start,end}-{time,index} commandline options,
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

(defun main ()
  "Entry point function of the bag-play program."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (let ((*readtable* (copy-readtable *readtable*)))
    (local-time:enable-read-macros)
    (process-commandline-options
     :version         (cl-rsbag-tools-play-system:version/list)
     :more-versions   (list :rsbag         (cl-rsbag-system:version/list)
			    :rsbag-tidelog (cl-rsbag-system:version/list))
     :update-synopsis #'update-synopsis
     :return          #'(lambda () (return-from main))))

  (unless (<= 1 (length (remainder)) 2)
    (error "~@<Specify input file and, optionally, base URI.~@:>"))

  (with-logged-warnings

    ;; Create a reader and start the receiving and printing loop.
    (bind (((input &optional (base-uri "/")) (remainder))
	   (channel-specs    (iter (for channel next (getopt :long-name "channel"))
				   (while channel)
				   (collect channel)))
	   (channels         (or (make-channel-filter channel-specs) t))
	   ((:values start-time start-index end-time end-index)
	    (process-bounds-options))
	   (replay-strategy  (parse-instantiation-spec
			      (getopt :long-name "replay-strategy")))
	   (progress         (getopt :long-name "show-progress")))

      (when (and start-time start-index)
	(error "~@<The commandline options \"start-time\" and ~
\"start-index\" are mutually exclusive.~@:>"))
      (when (and end-time end-index)
	(error "~@<The commandline options \"end-time\" and ~
\"end-index\" are mutually exclusive.~@:>"))

      (log1 :info "Using ~:[~*all channels~;channels matching ~@<~@;~{~S~^, ~}~@:>~]"
	    (not (eq channels t)) channel-specs)
      (log1 :info "Using base-URI ~A" base-uri)

      (let ((connection (apply #'bag->events input base-uri
			       :channels        channels
			       :replay-strategy replay-strategy
			       (append (when start-time
					 (list :start-time start-time))
				       (when start-index
					 (list :start-index start-index))
				       (when end-time
					 (list :end-time end-time))
				       (when end-index
					 (list :end-index end-index))))))

	(log1 :info "Connection ~A" connection)

	(unwind-protect
	     (with-interactive-interrupt-exit ()
	       (replay connection (connection-strategy connection)
		       :progress (case progress
				   (:line #'print-progress))))
	  (close connection))

	(unless (eq progress :none)
	  (terpri *standard-output*))))))
