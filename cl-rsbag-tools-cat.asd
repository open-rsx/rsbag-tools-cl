;;;; rsbag-tools-cat.asd --- System definition for the bag-cat program.
;;;;
;;;; Copyright (C) 2011-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:cl-rsbag-tools-cat-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:cl-rsbag-tools-cat-system)

;;; Version stuff

(defparameter +version-major+ 0
  "Major component of version number.")

(defparameter +version-minor+ 19
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

(asdf:defsystem "cl-rsbag-tools-cat"
  :description "A tool that is to RSBag log files what UNIX cat is to text files."
  :license     "GPLv3"                  ; see COPYING file for details
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ("alexandria"
                "let-plus"
                (:version "log4cl"                        "1.1.1")

                "cl-ppcre" ; for regex-based channel selection

                (:version "cl-rsbag"                      #.(version/string :revision? nil))

                (:version "rsb"                           #.(version/string :revision? nil)) ; TODO(jmoringe): not really required, functionality-wise
                (:version "rsb-tools-common"              #.(version/string :revision? nil))
                (:version "rsb-formatting"                #.(version/string :revision? nil))
                (:version "rsb-formatting-and-rsb-common" #.(version/string :revision? nil))
                (:version "rsb-stats"                     #.(version/string :revision? nil))

                (:version "rsbag-tools-common"            #.(version/string))
                (:version "rsbag-tools-commands"          #.(version/string)))

  :components  ((:module     "bag-cat"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "main")))))
