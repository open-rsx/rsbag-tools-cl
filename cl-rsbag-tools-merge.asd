;;;; rsbag-tools-merge.asd --- System definition for merge program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage :cl-rsbag-tools-merge-system
  (:use
   :cl
   :asdf)

  (:export
   :version/list
   :version/string))

(cl:in-package :cl-rsbag-tools-merge-system)


;;; Version stuff
;;

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 10
  "Minor component of version number.")

(let* ((version-file (merge-pathnames "version.sexp" *load-truename*))
       stream)
  (when (probe-file version-file)
    (setf stream (open version-file)))

  (defparameter +version-revision+ (if stream (read stream) 0)
    "Revision component of version number.")

  (defparameter +version-commit+ (when stream (read stream))
    "Commit component of version number.")

  (when stream (close stream)))

(defun version/list (&key
		     (revision? t)
		     commit?)
  "Return a version of the form (MAJOR MINOR [REVISION [COMMIT]])
where REVISION and COMMIT are optional.

REVISION? controls whether REVISION should be included. Default
behavior is to include REVISION.

COMMIT? controls whether COMMIT should be included. Default behavior
is to not include COMMIT."
  (append (list +version-major+ +version-minor+)
	  (when revision? (list +version-revision+))
	  (when (and commit? +version-commit+)
	    (list +version-commit+))))

(defun version/string (&rest args
		       &key
		       revision?
		       commit?)
  "Return a version string of the form
\"MAJOR.MINOR[.REVISION[-.COMMIT]]\" where REVISION and COMMIT are
optional.

See `version/list' for details on keyword parameters."
  (declare (ignore revision? commit?))
  (format nil "~{~A.~A~^.~A~^-~A~}" (apply #'version/list args)))


;;; System definition
;;

(defsystem :cl-rsbag-tools-merge
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "A tool that merges one or more rsbag log file into a
new rsbag log file, potentially transforming channels and/or entries
in the process."
  :depends-on  (:alexandria
		:let-plus
		:cl-ppcre

		(:version :cl-rsbag      #.(version/string :revision? nil))

		(:version :cl-rsb-common #.(version/string :revision? nil)))
  :components  ((:module     "bag-merge"
	         :components ((:file       "package")
			      (:file       "protocol"
			       :depends-on ("package"))
			      (:file       "transcoding"
			       :depends-on ("package" "protocol"))
			      (:file       "main"
			       :depends-on ("package" "protocol"))))))
