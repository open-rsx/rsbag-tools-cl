;;; main.lisp --- Main function of the bag-info program.
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

(in-package :rsbag.tools.info)

(defun update-synopsis ()
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "BAG-FILE"
   :item    (make-text :contents (format nil "Display information about a ~
specified log file.

Currently, the following file formats are supported:~{~&+ ~
~4A (extension: \".~(~:*~A~)\")~}

Examples:

  bag-info /tmp/everything.tide"
					 (map 'list #'car (rsbag.backend:backend-classes))))
   :item    (make-common-options)
   :item    (defgroup (:header "Display Options")
	      (flag :long-name  "compute-sizes"
		    :short-name "s"
		    :description
		    "Compute the sizes of the content of the whole log file and individual channels. This may take some time for large files."))))

(defun channel-size (channel)
  "Return the size of the content of CHANNEL in bytes."
  (reduce #'+ channel :key #'length))

(defun main ()
  "Entry point function of the bag-info program."
  (update-synopsis)
  (process-commandline-options
   :version         (cl-rsbag-tools-info-system:version/list)
   :update-synopsis #'update-synopsis
   :return          #'(lambda () (return-from main)))

  (unless (length= 1 (remainder))
    (error "Specify exactly one log file."))

  (with-logged-warnings
    (bind (((input) (remainder))
	   (sizes?  (getopt :long-name "compute-sizes"))
	   (*print-right-margin* (or most-positive-fixnum (com.dvlsoft.clon::line-width context)))
	   (*print-miser-width*  *print-right-margin*))
      (with-bag (bag input :direction :input)
	(format t "File ~S~&~2T~<~@;~@{~@(~6A~): ~
~:[N/A~;~:*~,,',:D~]~^~&~}~:>~&~2T~@<~@;~:{Channel ~
~S~&~4T~@<~@;~{~@(~6A~): ~:[N/A~;~:*~,,',:D~]~^~&~}~:>~&~}~:>~&"
		input
		`(:events ,(reduce #'+ (bag-channels bag) :key #'length)
		  ,@(when sizes?
		      `(:size ,(reduce #'+ (bag-channels bag)
				       :key #'channel-size)))
		  :start  ,(rsbag::start bag)
		  :end    ,(rsbag::end   bag))
		(iter (for channel each (bag-channels bag))
		      (collect (list (channel-name channel)
				     `(:events ,(length channel)
				       ,@(when sizes?
					   `(:size   ,(channel-size channel)))
				       :start  ,(rsbag::start channel)
				       :end    ,(rsbag::end   channel))))))))))
