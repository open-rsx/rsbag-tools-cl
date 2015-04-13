;;;; play.lisp --- Tests for the play command class.
;;;;
;;;; Copyright (C) 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands.test)

(deftestsuite play-root (rsbag-tools-commands-root)
  ()
  (:documentation
   "Test suite for the `play' command."))

(addtest (play-root
          :documentation
          "Test construction of the `play' command.")
  construction

  (ensure-cases (initargs &optional expected)
      `(;; Some invalid cases with missing initargs.
        (()                                   missing-required-initarg) ; :input-files is missing
        ((:input-files          ("foo.tide")) missing-required-initarg) ; :destination is missing

        ;; Some invalid cases with incompatible initargs.
        ((:input-files          ("foo.tide")
          :destination          "/"
          :replay-strategy      ,(rsbag.rsb:make-replay-strategy
                                  :as-fast-as-possible)
          :replay-strategy-spec "as-fast-as-possible")
                                              incompatible-initargs)

        ;; These are Ok.
        ((:input-files          ("foo.tide")
          :destination          "/"
          :replay-strategy-spec "as-fast-as-possible"))
        ((:input-files          (,#P"foo.tide")
          :destination          "/"
          :replay-strategy-spec "as-fast-as-possible"))
        ((:input-files          ("foo.tide")
          :destination          ,(puri:uri "/")
          :replay-strategy-spec "as-fast-as-possible"))
        ((:input-files          ("foo.tide")
          :destination          ,(rsb:make-scope "/")
          :replay-strategy-spec "as-fast-as-possible"))
        ((:input-files          ("foo.tide")
          :destination          "/"
          :replay-strategy      ,(rsbag.rsb:make-replay-strategy
                                  :as-fast-as-possible)))
        ((:input-files          ("foo.tide")
          :destination          "/"
          :replay-strategy-spec "as-fast-as-possible"
          :start-index          0))
        ((:input-files          ("foo.tide")
          :destination          "/"
          :replay-strategy-spec "as-fast-as-possible"
          :end-index            0))
        ((:input-files          ("foo.tide")
          :destination          "/"
          :replay-strategy-spec "as-fast-as-possible"
          :num-repetitions      1)))

    (let+ (((&flet do-it () (apply #'rsb.tools.commands:make-command :play
                                   :service 'rsbag.tools.commands::command
                                   initargs))))
      (case expected
        (missing-required-initarg
         (ensure-condition missing-required-initarg (do-it)))
        (incompatible-initargs
         (ensure-condition incompatible-initargs (do-it)))
        (t
         (ensure (typep (princ-to-string (do-it)) 'string)))))))
