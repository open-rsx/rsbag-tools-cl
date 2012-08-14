;;; main.lisp --- Main function of the bag-cat program.
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

(in-package :rsbag.tools.cat)


;;; Output protocol and implementation
;;

(defgeneric output (datum sink)
  (:documentation
   "Output DATUM to SINK."))

(defmethod output ((datum simple-array) (sink stream))
  (check-type datum (simple-array (unsigned-byte 8) (*)) "an octet-vector")

  (write-sequence datum sink))

(defmethod output ((datum string) (sink stream))
  (fresh-line sink)
  (write-sequence datum sink))

(defmethod output ((datum event) (sink stream))
  (output (event-data datum) sink))


;;; Help and main functions
;;

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (format nil
	  "Outputs data from channels in the bag file ~
INPUT-FILE-OR-- (or standard input, if \"-\" is specified) on standard ~
output.

The file format of INPUT-FILE is guessed based on the ~
filename. Currently, the following file formats are supported:~{~&+ ~
~4A (extension: \".~(~:*~A~)\")~}"
	  (map 'list #'car (rsbag.backend:backend-classes))))

(defun make-examples-string ()
  "Make and return a string containing usage examples of the program."
  (format nil
	  "~A /tmp/everything.tide

  Output the data from all channels in the log file ~
\"/tmp/everything.tide\" ordered by timestamps.

~:*~A -c isr /tmp/nao.tide

  Output the data from all channels of the log file \"/tmp/nao.tide\" ~
the names of which contain the string \"isr\".

~:*~A --channel 'STRING$' --channel 'BYTES$' log.tide

  Output the data from all channels of the log file \"log.tide\" ~
whose names end in either \"STRING\" or \"BYTES\".
"
	  "bag-cat"))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "INPUT-FILE-OR--"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   :item    (defgroup (:header "Printing Options")
	      (stropt  :long-name     "entry-separator"
		       :short-name    "p"
		       :argument-name "STRING-OR-KEYWORD"
		       :default-value ":newline"
		       :description
		       "A string that should be printing between each pair of entries. The special values :none and :newline disable printing of entry separators and cause a newline character to be used as entry separator respectively."))
   :item    (defgroup (:header "Selection Options")
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
		       "Mutually exclusive with --end-time."))
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

(defun main ()
  "Entry point function of the bag-play program."
  (update-synopsis)
  (local-time:enable-read-macros)
  (process-commandline-options
   :version         (cl-rsbag-tools-cat-system:version/list)
   :more-versions   (list :rsbag         (cl-rsbag-system:version/list)
			  :rsbag-tidelog (cl-rsbag-system:version/list))
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))

  (unless (length= 1 (remainder))
    (error "~@<Specify input file.~@:>"))

  (with-logged-warnings

    ;; Create a reader and start the receiving and printing loop.
    (bind ((input           (first (remainder)))
	   (entry-separator (when-let ((string (getopt :long-name "entry-separator")))
			      (if (starts-with #\: string)
				  (read-from-string string)
				  string)))
	   (start-time      (getopt :long-name "start-time"))
	   (start-index     (getopt :long-name "start-index"))
	   (end-time        (getopt :long-name "end-time"))
	   (end-index       (getopt :long-name "end-index"))
	   (specs           (iter (for channel next (getopt :long-name "channel"))
				  (while channel)
				  (collect channel)))
	   (channels        (or (make-channel-filter specs) t)))

      (check-type entry-separator (or string (member :none :newline))
		  ":none, :newline or a string not starting with \":\"")

      (when (and start-time start-index)
	(error "~@<The commandline options \"start-time\" and ~
\"start-index\" are mutually exclusive.~@:>"))
      (when (and end-time end-index)
	(error "~@<The commandline options \"end-time\" and ~
\"end-index\" are mutually exclusive.~@:>"))

      (log1 :info "Using ~:[~*all channels~;channels matching ~@<~@;~{~S~^, ~}~@:>~]"
	    (not (eq channels t)) specs)

      (with-interactive-interrupt-exit ()
	(with-bag (bag input :direction :input)
	  (bind ((predicate (if (eq channels t) (constantly t) channels))
		 (channels  (remove-if-not predicate (bag-channels bag)))
		 (sequence  (make-serialized-view channels)))
	    (macrolet
		((do-it (&optional end-index)
		   `(iter (for datum each sequence
			       :from (or start-index 0)
			       ,@(when end-index `(:to ,end-index)))
			  (unless (first-iteration-p)
			    (cond
			      ((stringp entry-separator)
			       (princ entry-separator *standard-output*))
			      ((eq entry-separator :newline)
			       (terpri *standard-output*))))

			  (output datum *standard-output*))))
	      (if end-index
		  (do-it end-index)
		  (do-it)))))))))
