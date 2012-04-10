;;; main.lisp --- Main function of the bag-record program.
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

(in-package :rsbag.tools.record)

(defun invoke-with-control-service (uri connection thunk)
  "Expose an RPC server at URI that allows remote clients to control
CONNECTION while THUNK executes."
  (log1 :info "Exposing control interface at URI ~A" uri)
  (let ((thread (bt:current-thread)))
    (with-local-server (server uri)
      (with-methods (server)
	  (("start" ()
	     (log1 :info "Starting recording")
	     (start connection)
	     (values))
	   ("stop" ()
	     (log1 :info "Stopping recording")
	     (stop connection)
	     (values))
	   ("terminate" ()
	     (log1 :info "Terminating")
	     (interrupt thread)
	     (values)))
	(funcall thunk)))))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   ;; Basic usage and specific options.
   :postfix "[URIS]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (defgroup (:header "Recording Options")
	      (path    :long-name     "output-file"
		       :short-name    "o"
		       :type          :file
		       :description
		       (format nil "Name of the file into which captured events should be written. The file format is determined based on the file type (extension). Currently, the following file formats are supported:~{~&+ ~4A (extension: \".~(~:*~A~)\")~}."
			       (map 'list #'car (rsbag.backend:backend-classes))))
	      (stropt  :long-name     "channel-allocation"
		       :short-name    "a"
		       :default-value "scope-and-type"
		       :argument-name "SPEC"
		       :description
		       (make-channel-strategy-help-string :show show))
	      (stropt  :long-name     "filter"
		       :short-name    "f"
		       :argument-name "SPEC"
		       :description
		       (make-filter-help-string :show show))
	      (stropt  :long-name     "control-uri"
		       :short-name    "c"
		       :argument-name "URI"
		       :description
		       "A URI specifying the root scope and transport configuration of an RPC server exposing methods which allow controlling the recording process. Currently, the following methods are provided:

+ start : void -> void
  Restart recording after it has been stopped.
+ stop : void -> void
  Stop recording allowing it to be restarted later.
+ terminate : void -> void
  Terminate the recording process and the program.")
	      (lispobj :long-name     "max-buffer-size"
		       :typespec      'positive-integer
		       :description
		       "The maximum amount of unwritten data that can be accumulated in memory before a disk write is forced. NOT IMPLEMENTED YET.")
	      (lispobj :long-name     "max-buffer-count"
		       :typespec      'positive-integer
		       :description
		       "The maximum number of unwritten events that can be accumulated in memory before a disk write is forced. NOT IMPLEMENTED YET.")
	      (lispobj :long-name     "max-buffer-time"
		       :typespec      'positive-real
		       :description
		       "The maximum age unwritten data in memory can reach before a disk write is forced. NOT IMPLEMENTED YET."))
   ;; Append RSB options.
   :item    (make-options
	     :show? (or (eq show t)
			(and (listp show) (member :rsb show))))
   ;; Append IDL options.
   :item    (make-idl-options)
   ;; Append examples.
   :item    (defgroup (:header "Examples")
	      (make-text :contents (make-examples-string)))))

(defun main ()
  "Entry point function of the bag-record program."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsbag-tools-record-system:version/list)
   :more-versions   (list :rsbag         (cl-rsbag-system:version/list)
			  :rsbag-tidelog (cl-rsbag-system:version/list))
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))

  (unless (remainder)
    (error "~@<Specify at least one URI from which events should be ~
recorded.~@:>"))

  (with-logged-warnings
    ;; Load IDLs as specified on the commandline.
    (process-idl-options)

    (rsb.formatting:with-print-limits (*standard-output*)
	;; Create a reader and start the receiving and printing loop.
	(bind ((control-uri   (when-let ((string (getopt :long-name "control-uri")))
				(puri:parse-uri string)))
	       (uris          (map 'list #'puri:parse-uri (remainder)))
	       (output        (or (getopt :long-name "output-file")
				  (error "~@<Specify output file.~@:>")))
	       (channel-alloc (parse-instantiation-spec
			       (getopt :long-name "channel-allocation")))
	       (filters       (iter (for spec next (getopt :long-name "filter"))
				    (while spec)
				    (collect (apply #'rsb.filter:filter
						    (parse-instantiation-spec spec)))))
	       (connection    (events->bag uris output
					   :channel-strategy channel-alloc
					   :filters          filters
					   :start?           (not control-uri)))
	       ((:flet recording-loop ())
		(unwind-protect
		     (with-interactive-interrupt-exit ()
		       (iter (sleep 10)
			     (format t "~A ~@<~@;~{~A~^, ~}~@:>~%"
				     (local-time:now)
				     (bag-channels (connection-bag connection)))))
		  (close connection))))

	  (log1 :info "Using URIs ~@<~@;~{~A~^, ~}~@:>" uris)
	  (if control-uri
	      (invoke-with-control-service
	       control-uri connection #'recording-loop)
	      (recording-loop))))))
