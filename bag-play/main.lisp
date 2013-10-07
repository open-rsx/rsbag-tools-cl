;;;; main.lisp --- Main function of the bag-play program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.tools.play)

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "INPUT-FILE [BASE-URI]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (make-replay-options :show show)
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
	   (mapcar #'(lambda (spec)
		       #'(lambda (channel)
			   (cl-ppcre:scan spec (channel-name channel))))
		specs))))

(defun print-progress (progress
		       index start-index end-index
		       timestamp)
  "Print the progress of the current replay onto the stream that is
the value of `*info-output*'."
  (format *info-output* "~C~A ~6,2,2F % ~9:D [~9:D,~9:D]"
	  #\Return
	  timestamp
	  progress
	  index start-index end-index)
  (force-output *info-output*))

(defun main ()
  "Entry point function of the bag-play program."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (let ((*readtable* (copy-readtable *readtable*)))
    (local-time:enable-read-macros)
    (process-commandline-options
     :version         (cl-rsbag-tools-play-system:version/list :commit? t)
     :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
			    :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
     :update-synopsis #'update-synopsis
     :return          #'(lambda () (return-from main))))

  (unless (<= 1 (length (remainder)) 2)
    (error "~@<Specify input file and, optionally, base URI.~@:>"))

  (with-logged-warnings

      ;; Create a reader and start the receiving and printing loop.
      (let+ ((error-policy (maybe-relay-to-thread
			    (process-error-handling-options)))
             ((input &optional (base-uri "/")) (remainder))
	     (channel-specs    (iter (for channel next (getopt :long-name "channel"))
				     (while channel)
				     (collect channel)))
	     (channels         (or (make-channel-filter channel-specs) t))
	     ((&values start-time start-index end-time end-index)
	      (process-bounds-options))
	     (replay-strategy  (parse-instantiation-spec
				(getopt :long-name "replay-strategy")))
	     (progress         (getopt :long-name "show-progress")))

	(log:info "~@<Using ~:[~*all channels~;channels matching ~@<~@;~{~S~^, ~}~@:>~]~@:>"
		  (not (eq channels t)) channel-specs)
	(log:info "~@<Using base-URI ~A~@:>" base-uri)

	(with-interactive-interrupt-exit ()
	  (with-error-policy (error-policy)
	    (let ((connection (apply #'bag->events input base-uri
				     :channels        channels
				     :replay-strategy replay-strategy
				     (append
				      (when start-time
					(list :start-time start-time))
				      (when start-index
					(list :start-index start-index))
				      (when end-time
					(list :end-time end-time))
				      (when end-index
					(list :end-index end-index))))))
	      (setf (rsb.ep:processor-error-policy connection)
		    error-policy)
	      (log:info "~@<Connection ~A~@:>" connection)
	      (unwind-protect
		   (replay connection (connection-strategy connection)
			   :progress (when *info-output*
				       (case progress
					 (:line #'print-progress))))
		(close connection)))))

	(unless (eq progress :none)
	  (terpri *standard-output*)))))
