;;;; introspect.lisp --- Tests for the introspect command class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands.test)

(deftestsuite introspect-root (rsbag-tools-commands-root)
  ()
  (:documentation
   "Test suite for the `introspect' command."))

(addtest (introspect-root
          :documentation
          "Test construction of the `introspect' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases with missing initargs.
        (()                                            missing-required-initarg) ; :input-files is missing

        ((:input-files          ("foo.tide")
          :style                ,(rsb.formatting:make-style
                                  :object-tree :service 'rsb.formatting.introspection::style)
          :style-spec            "object-tree")
                                                       incompatible-initargs)
        ((:input-files          ("foo.tide")
          :style-spec           "object-tree"
          :end-index            0
          :end-time             -1)
                                                       incompatible-initargs)

        ;; These are Ok.
        ((:input-files          ("foo.tide")
          :style-spec            "object-tree"))
        ((:input-files          ("foo.tide")
          :style-spec            "object-tree"
          :end-index            10))
        ((:input-files          ("foo.tide")
          :style-spec            "object-tree"
          :end-time             -5.0))
        ((:input-files          ("foo.tide")
          :style-spec            "object-tree :stateful? t")))

    (let+ (((&flet do-it () (apply #'rsb.tools.commands:make-command :introspect
                                   :service 'rsbag.tools.commands::command
                                   initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))
