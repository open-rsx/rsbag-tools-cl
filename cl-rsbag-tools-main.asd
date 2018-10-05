;;;; rsbag-tools-main.asd --- System definition for the bag-main program.
;;;;
;;;; Copyright (C) 2011-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:cl-rsbag-tools-main-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package :cl-rsbag-tools-main-system)

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

(defsystem :cl-rsbag-tools-main
  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :version     #.(version/string)
  :license     "GPLv3" ; see COPYING file for details
  :description "Main program and dispatch functions for all cl-rsbag
tools."
  :depends-on  (:alexandria

                (:version :rsb-introspection         #.(version/string :revision? nil))

                (:version :cl-rsbag-tools-record     #.(version/string))
                (:version :cl-rsbag-tools-info       #.(version/string))
                (:version :cl-rsbag-tools-cat        #.(version/string))
                (:version :cl-rsbag-tools-merge      #.(version/string))
                (:version :cl-rsbag-tools-play       #.(version/string))
                (:version :cl-rsbag-tools-introspect #.(version/string)))
  :components  ((:module     "main"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "main"))))
  :entry-point "rsbag.tools.main:main"
  :output-files (program-op (operation component)
                            (let* ((output/relative #-win32 "rsbag" #+win32 "rsbag.exe")
                                   (output/absolute (uiop:ensure-absolute-pathname
                                                     output/relative *default-pathname-defaults*)))
                              (values (list output/absolute) t))))

(defmethod perform :before ((operation program-op)
                            (component (eql (find-system :cl-rsbag-tools-main))))
  (mapc #'register-immutable-system (already-loaded-systems)))
