;;;; transform.lisp --- Tests for the transform command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands.test)

(deftestsuite transform-root (rsbag-tools-commands-root)
  ()
  (:documentation
   "Test suite for the `transform' command."))

(addtest (transform-root
          :documentation
          "Test construction of the `transform' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases with missing initargs.
        (()                                   missing-required-initarg) ; :input-files is missing
        ((:input-files          ("foo.tide")) missing-required-initarg) ; :output-file is missing

        ;; These are Ok.
        ((:input-files          ("foo.tide")
          :output-file          "bar.tide"))
        ((:input-files          (,#P"foo.tide")
          :output-file          "bar.tide"))
        ((:input-files          ("foo.tide")
          :output-file          ,#P"bar.tide"))
        ((:input-files          ("foo.tide")
          :output-file          "bar.tide"
          :start-index          0))
        ((:input-files          ("foo.tide")
          :output-file          "bar.tide"
          :end-index            0)))

    (let+ (((&flet do-it () (apply #'rsb.tools.commands:make-command :transform
                                   :service 'rsbag.tools.commands::command
                                   initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))
