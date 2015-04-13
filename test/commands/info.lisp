;;;; info.lisp --- Tests for the info command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands.test)

(deftestsuite info-root (rsbag-tools-commands-root)
  ()
  (:documentation
   "Test suite for the `info' command."))

(addtest (info-root
          :documentation
          "Test construction of the `info' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases with missing initargs.
        (()                               missing-required-initarg) ; :input-files is missing

        ;; These are Ok.
        ((:input-files   ("foo.tide")))
        ((:input-files   (,#P"foo.tide")))
        ((:input-files   ("foo.tide")
          :stream        ,*standard-output*))
        ((:input-files   ("foo.tide")
          :stream-spec   :error-output)))

    (let+ (((&flet do-it () (apply #'rsb.tools.commands:make-command :info
                                   :service 'rsbag.tools.commands::command
                                   initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))
