;;;; record.lisp --- Tests for the record command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands.test)

(deftestsuite record-root (rsbag-tools-commands-root)
  ()
  (:documentation
   "Test suite for the `record' command."))

(addtest (record-root
          :documentation
          "Test construction of the `record' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases with missing initargs.
        (()            missing-required-initarg) ; :uris is missing
        ((:uris ("/")) missing-required-initarg) ; :output-file is missing

        ;; Some invalid cases with incompatible initargs.
        ((:uris                    ("/")
          :output-file             "foo.tide"
          :channel-allocation      ,(rsbag.rsb:make-channel-strategy :scope-and-type)
          :channel-allocation-spec "scope-and-type")
         incompatible-initargs)
        ((:uris                    ("/")
          :output-file             "foo.tide"
          :flush-strategy          ,(rsbag.backend:make-flush-strategy :and)
          :flush-strategy-spec     "and")
         incompatible-initargs)

        ;; These are Ok.
        ((:uris                    ("/")
          :output-file             "foo.tide"))
        ((:uris                    (,(puri:uri "/"))
          :output-file             "foo.tide"))
        ((:uris                    (,(rsb:make-scope "/"))
          :output-file             "foo.tide"))
        ((:uris                    ("/")
          :output-file             ,#P"foo.tide"))
        ((:uris                    ("/")
          :output-file             "foo.tide"
          :index-timestamp         :create))
        ((:uris                    ("/")
          :output-file             "foo.tide"
          :channel-allocation      ,(rsbag.rsb:make-channel-strategy :scope-and-type)))
        ((:uris                    ("/")
          :output-file             "foo.tide"
          :channel-allocation-spec "scope-and-type"))
        ((:uris                    ("/")
          :output-file             "foo.tide"
          :flush-strategy          ,(rsbag.backend:make-flush-strategy :and)))
        ((:uris                    ("/")
          :output-file             "foo.tide"
          :flush-strategy-spec     "and"))
        ((:uris                    ("/")
          :control-uri             ,(puri:uri "/control")))
        ((:uris                    ("/")
          :output-file             "foo.tide"
          :control-uri             ,(puri:uri "/control"))))

    (let+ (((&flet do-it () (apply #'rsb.tools.commands:make-command :record
                                   :service 'rsbag.tools.commands::command
                                   initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))
