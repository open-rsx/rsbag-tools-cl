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

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "INPUT-FILE BASE-URI"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options)
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
	      (enum    :long-name     "strategy"
		       :short-name    "r"
		       :enum          (map 'list #'first (replay-strategy-classes))
		       :argument-name "STRATEGY"
		       :description
		       "Replay events form the specified input file according to STRATEGY."))
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

  (with-logged-warnings

    ;; Create a reader and start the receiving and printing loop.
    (bind (((input base-uri) (remainder))
	   (start-time  (getopt :long-name "start-time"))
	   (start-index (getopt :long-name "start-index"))
	   (end-time    (getopt :long-name "end-time"))
	   (end-index   (getopt :long-name "end-index"))
	   (channels    (or (make-channel-filter
			     (iter (for channel next (getopt :long-name "channel"))
				   (while channel)
				   (collect channel)))
			    t)))

      (when (and start-time start-index)
	(error "~@<The commandline options \"start-time\" and ~
\"start-index\" are mutually exclusive.~@:>"))
      (when (and end-time end-index)
	(error "~@<The commandline options \"end-time\" and ~
\"end-index\" are mutually exclusive.~@:>"))

      (log1 :info "Using ~:[~*all channels~;channels matching ~@<~@;~{~S~^, ~}~@:>~]"
	    (not (eq channels t)) channels)
      (log1 :info "Using base-URI ~A" base-uri)

      (let ((connection (apply #'bag->events input base-uri
			       :channels channels
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
	  (replay connection (connection-strategy connection)))

	(close connection)))))
