;;; main.lisp --- Main function of the bag-cat program.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
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

(cl:in-package :rsbag.tools.cat)


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
	  (mapcar #'car (rsbag.backend:backend-classes))))

(defun make-examples-string ()
  "Make and return a string containing usage examples of the program."
  (format nil
	  "~2T~A /tmp/everything.tide

Output the data from all channels in the log file ~
\"/tmp/everything.tide\" ordered by timestamps.

~2T~:*~A -c isr /tmp/nao.tide

Output the data from all channels of the log file \"/tmp/nao.tide\" ~
the names of which contain the string \"isr\".

~2T~:*~A --channel 'STRING$' --channel 'BYTES$' log.tide

Output the data from all channels of the log file \"log.tide\" whose ~
names end in either \"STRING\" or \"BYTES\".

~2T~:*~A --style 'payload :separator (#\\Newline (:rule #\\-))' log.tide

Print event payloads separating payloads of different events by ~
newlines and horizontal rules.

~2T~:*~A --style 'programmable/template :template ~
#P\"my-template-file.template\"' log.tide

Format events in the log file \"log.tide\" by applying the template ~
in \"my-template-file.template\" to each event. See output of ~
--help-for styles for more information."
	  "bag-cat"))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "INPUT-FILE-OR--"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (make-replay-options :show show
				 :replay-strategy-default "as-fast-as-possible")
   :item    (defgroup (:header "Output Options")
	      (stropt  :long-name     "style"
		       :default-value "payload"
		       :argument-name "SPEC"
		       :description
		       (make-style-help-string :show show))
	      (enum    :long-name     "target-stream"
		       :enum          '(:stdout :standard-output
					:stderr :error-output)
		       :default-value :standard-output
		       :argument-name "STREAM-NAME"
		       :description
		       "Stream to which produced output should be sent."))
   ;; Append IDL options.
   :item    (make-idl-options)
   ;; Append examples.
   :item    (defgroup (:header "Examples")
	      (make-text :contents (make-examples-string)))))

(defun make-channel-filter (specs)
  (when specs
    (apply #'disjoin
	   (mapcar #'(lambda (spec)
		       #'(lambda (channel)
			   (cl-ppcre:scan spec (channel-name channel))))
		   specs))))

(defun main ()
  "Entry point function of the bag-cat program."
  (update-synopsis)
  (let ((*readtable* (copy-readtable *readtable*)))
    (local-time:enable-read-macros)
    (process-commandline-options
     :version         (cl-rsbag-tools-cat-system:version/list :commit? t)
     :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
			    :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
     :update-synopsis #'update-synopsis
     :return          #'(lambda () (return-from main))))

  (unless (length= 1 (remainder))
    (error "~@<Specify input file.~@:>"))

  (with-print-limits (*standard-output*)
    (with-logged-warnings
      ;; Load IDLs as specified on the commandline.
      (process-idl-options)

      ;; Gather the following things from commandline options:
      ;; + input file(s)
      ;; + selected channels for replay
      ;; + temporal/index range for replay
      ;; + formatting style
      ;; Pass all of these to `bag->events' for and start the
      ;; resulting connection.
      (let+ ((error-policy  (maybe-relay-to-thread
			     (process-error-handling-options)))
	     (input         (first (remainder)))
	     (channel-specs (iter (for channel next (getopt :long-name "channel"))
				  (while channel)
				  (collect channel)))
	     (channels      (or (make-channel-filter channel-specs) t))
	     ((&values start-time start-index end-time end-index)
	      (process-bounds-options))
	     (replay-strategy (parse-instantiation-spec
			       (getopt :long-name "replay-strategy")))
	     (style           (let+ (((class &rest args)
				      (parse-instantiation-spec
				       (getopt :long-name "style"))))
				(apply #'make-instance (find-style-class class)
				       args)))
	     (target          (ecase (getopt :long-name "target-stream")
				((:stdout :standard-output) *standard-output*)
				((:stderr :error-output)    *error-output*)))
	     (sink            #'(lambda (datum)
				  (format-event datum style target))))
	(with-interactive-interrupt-exit ()
	  (with-error-policy (error-policy)
	    (let ((connection
		   (apply #'bag->events input sink
			  :channels        channels
			  :transform       `(&from-source
					     :converter ,(default-converter 'nibbles:octet-vector))
			  :replay-strategy replay-strategy
			  (append (when start-time
				    (list :start-time start-time))
				  (when start-index
				    (list :start-index start-index))
				  (when end-time
				    (list :end-time end-time))
				  (when end-index
				    (list :end-index end-index))))))
	      (setf (rsb.ep:processor-error-policy connection) error-policy)
	      (log:info "~@<Replaying using connection ~A~@:>" connection)
	      (unwind-protect
		   (replay connection (connection-strategy connection))
		(close connection)))))))))
