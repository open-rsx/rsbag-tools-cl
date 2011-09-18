;;; main.lisp --- Main function of the bag-record program.
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

(in-package :rsbag.tools.record)

(defun make-help-string ()
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Capture events being exchanged on the scopes ~
designated by URIS and store them in the specified output file. For ~
each URI, one channel is created in the output file. These channels ~
store the events exchanged in the corresponding RSB channels. Each URI ~
has to be of the form

  ")
    (print-uri-help stream)
    (format stream
	    "
Examples:

  ~A -o /tmp/everything.tide spread://azurit:4803/
  ~:*~A -o /tmp/nao.tide 'spread:/nao/vision/top?name=4803'
  ~:*~A -o /tmp/multichannel.tide 'spread:/nao/vision/top' 'spread:/nao/audio/all'
"
	    "bag-record")))

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "[URIS]"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   :item    (defgroup (:header "Recording Options")
	      (path    :long-name     "output-file"
		       :short-name    "o"
		       :type          :file
		       :description
		       (format nil "Name of the file into which captured events should be written. The file format is determined based on the file type (extension). Currently, the following file formats are supported:~{~&+ ~4A (extension: \".~(~:*~A~)\")~}."
			    (map 'list #'car (rsbag.backend:backend-classes))))
	      (enum    :long-name     "channel-allocation"
		       :short-name    "a"
		       :enum          (map 'list #'car (rsbag.rsb:channel-strategy-classes))
		       :default-value :scope-and-type
		       :argument-name "STRATEGY"
		       :description
		       (format nil "The strategy that should be use to ~
allocate channels in the output bag file for received ~
events. Currently, the following strategies are supported:~{~&+ ~A~}."
			       (map 'list #'car (rsbag.rsb:channel-strategy-classes))))
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
			(and (listp show) (member :rsb show))))))

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

  (with-logged-warnings

    ;; Create a reader and start the receiving and printing loop.
    (let* ((uris               (map 'list #'puri:uri (remainder)))
	   (output             (or (getopt :long-name "output-file")
				   (error "Specify output file")))
	   (channel-allocation (getopt :long-name "channel-allocation")))

      (log1 :info "Using URIs ~@<~@;~{~A~^, ~}~@:>" uris)

      (let* ((connection (events->bag uris output
				      :channel-strategy channel-allocation))
	     (*print-right-margin* (com.dvlsoft.clon::stream-line-width *standard-output*))
	     (*print-miser-width*  *print-right-margin*))

	(with-interactive-interrupt-exit ()
	  (iter (sleep 10)
		(format t "~A ~@<~@;~{~A~^, ~}~@:>~%"
			(local-time:now)
			(bag-channels (connection-bag connection)))))

	(close connection)))))
