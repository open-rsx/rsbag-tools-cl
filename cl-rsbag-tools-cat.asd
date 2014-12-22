;;;; rsbag-tools-cat.asd --- System definition for the bag-cat program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
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

(defparameter +version-minor+ 12
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

(defsystem :cl-rsbag-tools-cat
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details
  :description "A tool that is to RSBag log files what UNIX cat is to text files."
  :depends-on  (:alexandria
                :let-plus
                (:version :log4cl                        "1.1.1")

                :cl-ppcre ;; for regex-based channel selection

                (:version :cl-rsbag                      #.(version/string :revision? nil))
                (:version :cl-rsbag-common               #.(version/string))

                (:version :cl-rsb                        #.(version/string :revision? nil)) ; TODO(jmoringe): not really required, functionality-wise
                (:version :cl-rsb-common                 #.(version/string :revision? nil))
                (:version :cl-rsb-formatting             #.(version/string :revision? nil))
                (:version :rsb-formatting-and-rsb-common #.(version/string :revision? nil))
                (:version :cl-rsb-stats                  #.(version/string :revision? nil)))
  :components  ((:module     "bag-cat"
                 :components ((:file       "package")
                              (:file       "main"
                               :depends-on ("package"))))))
