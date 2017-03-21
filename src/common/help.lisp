;;;; help.lisp --- Automatic generation of help strings.
;;;;
;;;; Copyright (C) 2012-2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.common)

(defun make-replay-strategy-help-string (&key (show :default))
  "Return a help string that explains how to specify replay strategies
   and lists the available replay strategies."
  (with-output-to-string (stream)
    (format stream "Replay events form the specified input file ~
                    according to SPEC which has to be of the form~@
                    ~@
                    ~2@TKIND KEY1 VALUE1 KEY2 VALUE2 ...~@
                    ~@
                    where keys and values depend on KIND and are ~
                    optional in most cases. Examples (note that the ~
                    single quotes have to be included only when used ~
                    within a shell):~@
                    ~@
                    ~2@T--replay-strategy recorded-timing~@
                    ~2@T-r as-fast-as-possible~@
                    ~2@T--replay-strategy 'fixed-rate :rate 10'~@
                    ~2@T-r 'remote-controlled :uri ~
                      \"spread://localhost:4803/myplayback/control\"'~@
                    ~@
                    ")
    (with-abbreviation (stream :strategies show)
      (format stream "Currently, the following strategies are supported:~@
                      ~@
                      ")
      (print-classes-help-string
       (mapcar (lambda (provider)
                 (list (service-provider:provider-name provider)
                       (service-provider:provider-class provider)))
               (service-provider:service-providers 'rsbag.rsb.replay:strategy))
       stream
       :initarg-blacklist '(:start-index :end-index
                            :start-time  :end-time
                            :num-repetitions
                            :error-policy
                            :stream :previous-command)))))
