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

(defun update-synopsis (&key
			(show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "BASE-URI"
   :item    (make-text :contents (format nil "Capture events being exchanged on the scopes designated by URIS and store them in the specified output file. For each URI, one channel is created in the output file. These channels store the events exchanged in the corresponding RSB channels.

Currently, the following file formats are supported:~{~&+ ~4A (extension: \".~(~:*~A~)\")~}

Examples:

  bag-play --all /tmp/everything.tide spread://azurit:4803/
  bag-play -c /nao/vision/top /tmp/nao.tide 'spread:/nao/vision/top?name=4803'
"
					 (map 'list #'car (rsbag.backend:backend-classes))))
   :item    (make-common-options)
   :item    (defgroup (:header "Playback Options")
	      (flag :long-name   "all-channels"
		    :short-name  "a"
		    :description
		    "Replay data from all channels stored in the specified file."))
   ;; Append RSB options.
   :item    (make-options
	     :show? (or (eq show t)
			(and (listp show) (member :rsb show))))))

(defun main ()
  "Entry point function of the bag-play program."
  (update-synopsis)
  (setf *default-configuration* (options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsbag-tools-play-system:version/list)
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))

  (with-logged-warnings

    ;; Create a reader and start the receiving and printing loop.
    (bind (((input base-uri) (remainder))
	   (channels (if (getopt :long-name "all-channels")
			 :all
			 (iter (for channel next (getopt :long-name "channel"))
			       (while channel)
			       (collect channel)))))

      (log1 :info "Using channels ~A" channels)
      (log1 :info "Using base-URI ~A" base-uri)

      (let ((connection (bag->events input base-uri)))

	(with-interactive-interrupt-exit ()
	  (iter (sleep 10)
		(format t "~A~%" (local-time:now))))

	(close connection)))))
