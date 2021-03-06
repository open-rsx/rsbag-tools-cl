;;;; transform.lisp --- Tests for the transform command class.
;;;;
;;;; Copyright (C) 2015, 2017 Jan Moringen
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

        ;; Some invalid cases with incompatible initargs.
        ((:input-files             ("foo.tide")
          :output-file             "foo.tide"
          :channel-allocation      ,(rsbag.rsb.recording:make-strategy :scope-and-type)
          :channel-allocation-spec "scope-and-type")
         incompatible-initargs)

        ;; These are Ok.
        ((:input-files             ("foo.tide")
          :output-file             "bar.tide"))
        ((:input-files             (,#P"foo.tide")
          :output-file             "bar.tide"))
        ((:input-files             ("foo.tide")
          :output-file             ,#P"bar.tide"))
        ((:input-files             ("foo.tide")
          :output-file             "bar.tide"
          :start-index             0))
        ((:input-files             ("foo.tide")
          :output-file             "bar.tide"
          :end-index               0))
        ((:input-files             ("foo.tide" "baz.tide")
          :output-file             "bar.tide"))
        ((:input-files             ("foo.tide")
          :output-file             "bar.tide"
          :index-timestamp         nil))
        ((:input-files             ("foo.tide")
          :output-file             "bar.tide"
          :index-timestamp         :create))
        ((:input-files             ("foo.tide")
          :output-file             "bar.tide"
          :channel-allocation      ,(rsbag.rsb.recording:make-strategy :scope-and-type)))
        ((:input-files             ("foo.tide")
          :output-file             "bar.tide"
          :channel-allocation-spec "scope-and-type")))

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
