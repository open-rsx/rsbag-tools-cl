;;; main.lisp --- Main function of the bag-play program.
;;
;; Copyright (C) 2011 Jan Moringen
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

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Replay events from INPUT-FILE on RSB channels ~
derived from BASE-URI. BASE-URI which has to be of the form

  ")
    (print-uri-help stream)
    (format stream
	    "
The file format of INPUT-FILE is guessed based on the ~
filename. Currently, the following file formats are supported:~{~&+ ~
~4A (extension: \".~(~:*~A~)\")~}

Examples:

  ~A /tmp/everything.tide spread://azurit:4803/
  ~:*~A -c /nao/vision/top /tmp/nao.tide 'spread:/nao/vision/top?name=4803'
"
	    (map 'list #'car (rsbag.backend:backend-classes))
	    "bag-play")))

(defun make-replay-strategy-help-string ()
  "Return a help string that explains how to specify replay strategies
and lists the available replay strategies."
  (with-output-to-string (stream)
    (format stream "Replay events form the specified input file ~
according to STRATEGY. Each SPEC has to be of the form

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values depend on KIND and may be optional in some ~
cases. Examples (note that the single quotes have to be included only ~
when used within a shell):

  --replay-strategy recorded-timing
  -r as-fast-as-possible
  --replay-strategy 'fixed-rate :rate 10'

Currently, the following strategies are supported:

")
    (print-classes-help-string
     (replay-strategy-classes) stream
     :initarg-blacklist '(:start-index :end-index :error-policy))))

(defun parse-replay-strategy-spec (string)
  "Parse STRING as a filter specification of one of the forms

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

and return the result as a list."
  (with-input-from-string (stream string)
    (iter (for token in-stream stream)
	  (collect
	      (if (and (first-iteration-p)
		       (not (keywordp token)))
		  (make-keyword (string-upcase (string token)))
		  token)))))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "INPUT-FILE BASE-URI"
   :item    (make-text :contents (make-help-string))
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
		       "Start replaying events at the point in time indicated by TIMESTAMP-OR-SECONDS. When the value should be parsed as a timestamp, the syntax @[YYYY-MM-DDT]HH:MM:SS has to be used. A single real number is interpreted as time in seconds relative to the beginning of the replay. Mutually exclusive with --start-index. NOT IMPLEMENTED YET.")
	      (lispobj :long-name     "start-index"
		       :short-name    "S"
		       :typespec      'non-negative-integer
		       :argument-name "INDEX"
		       :description
		       "Mutually exclusive with --start-time.")
	      (lispobj :long-name     "end-time"
		       :short-name    "e"
		       :typespec      '(or real local-time:timestamp)
		       :argument-name "TIMESTAMP-OR-SECONDS"
		       :description
		       "Stop replaying events at the point in time indicated by TIMESTAMP-OR-SECONDS. When the value should be parsed as a timestamp, the syntax @[YYYY-MM-DDT]HH:MM:SS has to be used. A single real number is interpreted as time in seconds relative to the beginning of the replay. Mutually exclusive with --end-index. NOT IMPLEMENTED YET.")
	      (lispobj :long-name     "end-index"
		       :short-name    "E"
		       :typespec      'non-negative-integer
		       :argument-name "INDEX"
		       :description
		       "Mutually exclusive with --end-time.")
	      (stropt  :long-name     "replay-strategy"
		       :short-name    "r"
		       :default-value "recorded-timing"
		       :argument-name "STRATEGY"
		       :description
		       (make-replay-strategy-help-string))
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
			(and (listp show) (member :rsb show))))))

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
  (format *standard-output* "~C~A ~6,2F % ~9:D [~9:D,~9:D]"
	  #\Return
	  timestamp
	  (* 100 progress)
	  index start-index end-index)
  (force-output *standard-output*))

(defun main ()
  "Entry point function of the bag-play program."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (local-time:enable-read-macros)
  (process-commandline-options
   :version         (cl-rsbag-tools-play-system:version/list)
   :more-versions   (list :rsbag         (cl-rsbag-system:version/list)
			  :rsbag-tidelog (cl-rsbag-system:version/list))
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))

  (unless (length= 2 (remainder))
    (error "~@<Specify input file and base URI.~@:>"))

  (with-logged-warnings

    ;; Create a reader and start the receiving and printing loop.
    (bind (((input base-uri) (remainder))
	   (start-time       (getopt :long-name "start-time"))
	   (start-index      (getopt :long-name "start-index"))
	   (end-time         (getopt :long-name "end-time"))
	   (end-index        (getopt :long-name "end-index"))
	   (channel-specs    (iter (for channel next (getopt :long-name "channel"))
				   (while channel)
				   (collect channel)))
	   (channels         (or (make-channel-filter channel-specs) t))
	   ((name &rest args) (parse-replay-strategy-spec
			       (getopt :long-name "replay-strategy")))
	   (class            (find-replay-strategy-class name))
	   (replay-strategy  (apply #'make-instance class args))
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

	(with-interactive-interrupt-exit ()
	  (replay connection (connection-strategy connection)
		  :progress (case progress
			      (:line #'print-progress))))
	(unless (eq progress :none)
	  (terpri *standard-output*))

	(close connection)))))
