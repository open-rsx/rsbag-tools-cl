;;;; main.lisp --- Main function of the bag-merge program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.merge)

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (format nil "Copy entries from input files matching GLOB-EXPRESSION ~
or from an explicitly given list of INPUT-FILEs into the specified ~
output file. In addition to the canonical globbing syntax, expressions ~
of the form

  SOMESTUFF/**/MORESTUFF

can be used to search directories recursively.

The file formats of input files and the output file are determined ~
based on the file type (extension). Currently, the following file ~
formats are supported:~{~&+ ~4A (extension: \".~(~:*~A~)\")~}.
"
	  (mapcar #'car (rsbag.backend:backend-classes))))

(defun make-example-string (&key
			    (program-name "bag-merge" #+later (progname)))
  "Make and return a string containing usage examples of the program."
  (format nil "~A -o bla.tide '/vol/my-separate-logs/*.tide'

  Merge all log files matching the glob expression ~
\"/vol/my-separate-logs/*.tide\" into a single log file named ~
\"bla.tide\". Note the quotes which prevent the shell from ~
interpreting the glob expression.

"
	  program-name))

(defun update-synopsis (&key
		        (show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "GLOB-EXPRESSION | INPUT-FILE+"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Display Options")
	      (path    :long-name     "output-file"
		       :short-name    "o"
		       :type          :file
		       :description
		       (format nil "Name of the file into which ~
captured events should be written. The file format is determined based ~
on the file type (extension). Currently, the following file formats ~
are supported:~{~&+ ~4A (extension: \".~(~:*~A~)\")~}."
			       (mapcar #'car (rsbag.backend:backend-classes))))
	      (switch  :long-name     "force"
		       :default-value nil
		       :description
		       "Should the output file be overwritten in case it already exists?")
	      (stropt  :long-name     "channel"
		       :short-name    "c"
		       :argument-name "NAME-OR-REGEXP"
		       :description
		       "Select the channels matching NAME-OR-REGEXP for merging. This option can be specified multiple times.")
              (stropt  :long-name "transform-timestamp"
		       :argument-name "TIMESTAMP-NAME"
		       :description
		       "Index events by TIMESTAMP-NAME instead of the create timestamp."))
   :item    (defgroup (:header "Examples")
	      (make-text :contents (make-example-string)))))

(defun make-channel-filter (specs)
  (when specs
    (apply #'disjoin
	   (mapcar #'(lambda (spec)
		       #'(lambda (channel)
			   (cl-ppcre:scan spec (channel-name channel))))
		   specs))))

(defun collect-input-files (args)
  "Collect and return a list of input files according to ARGS.
ARGS can be
+ A designator of a wild pathname matching one or more files
+ A list of pathname designators of existing files"
  (let+ ((parsed (mapcar #'parse-namestring args))
	 ((&flet existing-file (pathname)
	    (when-let ((probed (probe-file pathname)))
	      (and (pathname-name probed) (pathname-type probed))))))
    (cond
      ;; Neither glob expression nor filenames
      ((null args)
       (error "~@<No glob expression or one or more input log files ~
specified.~@:>"))

      ;; A single glob expression. Does it match anything?
      ((and (length= 1 parsed) (wild-pathname-p (first parsed)))
       (or (directory (first parsed))
	   (error "~@<The specified input glob expression ~S did not ~
match any files.~@:>"
		  (first args))))

      ;; Multiple argument: should refer to existing files.
      ((when-let ((invalid (remove-if #'existing-file parsed)))
	 (error "~@<The following specified input file~P~:* ~
~[~;does~:;do~] not exist: ~{~S~^, ~}.~@:>"
		(length invalid) invalid)))

      ;; We don't understand anything else.
      (t
       parsed))))

(defun print-progress (index
		       &optional
		       length
		       input-bag  input-channel
		       output-bag output-channel)
  (case index
    ((t)
     (fresh-line *standard-output*))
    (t
     (format *standard-output* "~C[~28A|~48A] -> [~28A|~48A] ~6,2,2F % ~9:D/~9:D"
	     #\Return
	     input-bag  (channel-name input-channel)
	     output-bag (channel-name output-channel)
	     (/ (1+ index) (max 1 length)) index (1- length))
     (force-output *standard-output*))))

(defun main ()
  "Entry point function of the bag-merge program."
  (update-synopsis)
  (process-commandline-options
   :version         (cl-rsbag-tools-merge-system:version/list :commit? t)
   :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
			  :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))

  (with-logged-warnings
    (progn #+later with-print-limits
      (let+ ((error-policy    (maybe-relay-to-thread
			       (process-error-handling-options)))
	     (input-files     (collect-input-files (remainder)))
	     (output/pathname (or (getopt :long-name "output-file")
				  (error "~@<No output file specified.~@:>")))
	     (force           (getopt :long-name "force"))
	     (channel-specs   (iter (for channel next (getopt :long-name "channel"))
				    (while channel)
				    (collect channel)))
	     (channels        (or (make-channel-filter channel-specs) t))
	     (transform/timestamp (when-let ((spec (getopt :long-name "transform-timestamp")))
				    (read-from-string spec)))
	     inputs
	     output)

	(with-error-policy (error-policy)
	  ;; Since `with-bag' only handles one bag, we have to handle
	  ;; the possibility of unwinding with multiple open bags
	  ;; manually.
	  (unwind-protect
	       (progn
		 ;; Open files and store resulting bags successively
		 ;; to ensure that open bags can be closed when
		 ;; unwinding.
		 (iter (for file in input-files)
		       (format *standard-output* "Opening input file ~S~%" file)
		       (push (open-bag file :direction :input) inputs))
		 (format *standard-output* "Opening output file ~S~%"
			 output/pathname)
		 (setf output (open-bag
			       output/pathname
			       :if-exists (if force :supersede :error)))

		 ;; Transcode individual input files into output file.
		 (apply #'transcode inputs output
			:channels channels
			:progress #'print-progress
			(etypecase transform/timestamp
			  (null    nil)
			  (keyword
			   (list :transform/timestamp
				 #'(lambda (timestamp datum)
				     (rsb:timestamp datum transform/timestamp)))))))
	    (iter (for bag in (cons output inputs))
		  (when bag
		    (handler-case
			(close bag)
		      (error (condition)
			(warn "~@<Error closing bag ~A: ~A~@:>"
			      bag condition)))))))))))
