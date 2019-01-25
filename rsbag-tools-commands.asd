;;;; rsbag-tools-commands.asd --- System definition for RSBag commands.
;;;;
;;;; Copyright (C) 2014-2019 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:defpackage #:rsbag-tools-commands-system
  (:use
   #:cl
   #:asdf)

  (:export
   #:version/list
   #:version/string))

(cl:in-package #:rsbag-tools-commands-system)

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

(asdf:defsystem "rsbag-tools-commands"
  :description "RSBag commands."
  :license     "GPLv3"                 ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ("alexandria"
                "let-plus"
                (:version "more-conditions"               "0.4")
                (:version "log4cl"                        "1.1.1")
                (:version "architecture.service-provider" "0.1")
                (:version "utilities.print-items"         "0.1")

                (:version "cl-rsbag"                      #.(version/string :revision? nil))
                (:version "rsbag-builder"                 #.(version/string :revision? nil))

                (:version "rsb"                           #.(version/string :revision? nil))
                (:version "rsb-patterns-request-reply"    #.(version/string :revision? nil))

                (:version "rsb-tools-common"              #.(version/string :revision? nil))
                (:version "rsb-formatting"                #.(version/string :revision? nil))
                (:version "rsb-tools-commands"            #.(version/string :revision? nil)))

  :components  ((:module     "commands"
                 :pathname   "src/commands"
                 :serial     t
                 :components ((:file       "package")
                              (:file       "protocol")

                              (:file       "util")
                              (:file       "mixins")

                              (:file       "redump")
                              (:file       "info")
                              (:file       "record")
                              (:file       "play")
                              (:file       "cat")
                              (:file       "transform")
                              (:file       "introspect"))))
  :in-order-to ((test-op (test-op "rsbag-tools-commands/test"))))

(asdf:defsystem "rsbag-tools-commands/test"
  :description "Unit tests for rsbag-tools-commands system."
  :license     "GPLv3"                 ; see COPYING file for details.

  :author      "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"
  :maintainer  "Jan Moringen <jmoringe@techfak.uni-bielefeld.de>"

  :version     #.(version/string)
  :depends-on  ("alexandria"
                "let-plus"

                (:version "lift"                 "1.7.1")

                (:version "rsbag-tools-commands" #.(version/string)))

  :components  ((:module     "commands"
                 :pathname   "test/commands"
                 :serial     t
                 :components ((:file       "package")

                              (:file       "info")
                              (:file       "record")
                              (:file       "play")
                              (:file       "cat")
                              (:file       "transform")
                              (:file       "introspect"))))

  :perform     (test-op (operation component)
                 (funcall (find-symbol "RUN-TESTS" :lift)
                          :config (funcall (find-symbol "LIFT-RELATIVE-PATHNAME" :lift)
                                           "lift-rsbag-tools-commands.config"))))
