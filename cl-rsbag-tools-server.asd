;;;; rsbag-tools-server.asd --- System definition for the bag-server program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:cl-rsbag-tools-server-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:cl-rsbag-tools-server-system)

;;; Version stuff

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

   COMMIT? controls whether COMMIT should be included. Default
   behavior is to not include COMMIT."
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

(defsystem :cl-rsbag-tools-server
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPL3; see COPYING file for details."
  :description "A tool that serves the contents of rsbag log files by means of a TODO RESTful interface or web-interface."
  :depends-on  (:alexandria
                (:version :let-plus      "0.2")
                (:version :iterate       "1.4.4")
                :com.dvlsoft.clon

                :cl-json                                     ; for JSON formatting style
                :cxml                                        ; for XML formatting style

                (:version :cl-rsbag      #.(version/string :revision? nil))
                (:version :rsbag-web     #.(version/string :revision? nil))

                (:version :cl-rsb        #.(version/string :revision? nil))
                (:version :cl-rsb-common #.(version/string :revision? nil))
                (:version :rsb-web       "0.1.0"))
  :components  ((:module     "bag-server"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "help")
                              (:file       "main")))))
