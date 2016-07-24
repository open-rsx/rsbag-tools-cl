;;;; cat.lisp --- Tests for the cat command class.
;;;;
;;;; Copyright (C) 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands.test)

(deftestsuite cat-root (rsbag-tools-commands-root)
  ()
  (:documentation
   "Test suite for the `cat' command."))

(addtest (cat-root
          :documentation
          "Test construction of the `cat' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases with missing initargs.
        (()                                            missing-required-initarg) ; :input-files is missing
        ((:input-files          ("foo.tide"))          missing-required-initarg) ; :replay-strategy is missing
        ((:input-files          ("foo.tide")
          :replay-strategy-spec "as-fast-as-possible") missing-required-initarg) ; :style[-spec] is missing

        ;; Some invalid cases with incompatible initargs.
        ((:input-files          ("foo.tide")
          :replay-strategy      ,(rsbag.rsb:make-replay-strategy
                                  :as-fast-as-possible)
          :replay-strategy-spec "as-fast-as-possible")
                                                       incompatible-initargs)
        ((:input-files          ("foo.tide")
          :replay-strategy-spec "as-fast-as-possible"
          :style                ,(rsb.formatting:make-style :detailed)
          :style-spec            "detailed")
                                                       incompatible-initargs)

        ;; These are Ok.
        ((:input-files          ("foo.tide")
          :replay-strategy-spec "as-fast-as-possible"
          :style-spec            "detailed"))
        ((:input-files          (,#P"foo.tide")
          :replay-strategy-spec "as-fast-as-possible"
          :style-spec            "detailed"))
        ((:input-files          ("foo.tide")
          :replay-strategy      ,(rsbag.rsb:make-replay-strategy
                                  :as-fast-as-possible)
          :style-spec           "detailed"))
        ((:input-files          ("foo.tide")
          :replay-strategy      "as-fast-as-possible"
          :style-spec           "detailed"
          :progress-style       :line)))

    (let+ (((&flet do-it () (apply #'rsb.tools.commands:make-command :cat
                                   :service 'rsbag.tools.commands::command
                                   initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))
