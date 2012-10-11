;;;; help.lisp --- Help text generation for the bag-server program.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.server)

(defun make-help-string (&key
                         (show :default))
  "Return a help that explains the commandline option interface."
  (declare (ignore show))

  (with-output-to-string (stream)
    (format stream "Serve events from the log files designated by ~
                    INPUT-FILE-OR-GLOB-PATTERN-N under the URLs~@
                    ~@
                    ~2@Thttp://BIND-ARRDRESS:PORT/~@
                    ~2@Thttp://BIND-ARRDRESS:PORT/BAG-1~@
                    ~2@Thttp://BIND-ARRDRESS:PORT/BAG-2~@
                    ~2@T...~@
                    ~@
                    where BAG-N are the basenames of the individual ~
                    log files designated by ~
                    INPUT-FILE-OR-GLOB-PATTERN-N and BIND-ADDRESS and ~
                    PORT are specified via the --address and --port ~
                    commandline options. In addition to the canonical ~
                    globbing syntax, expressions of the form~@
                    ~@
                    ~2@TSOMESTUFF/**/MORESTUFF~@
                    ~@
                    can be used to search directories recursively.~@
                    ~@
                    The file formats of the input files are guessed ~
                    based on the filename. Currently, the following ~
                    file formats are supported:~{~&+ ~4A (extension: ~
                    \".~(~:*~A~)\")~}"
            (mapcar #'car (rsbag.backend:backend-classes)))))

(defun make-examples-string (&key (program-name "bag-server"))
  "Make and return a string containing usage examples of the program."
  (format nil "~2@T~A /vol/studies/slots/*/*.tide~@
               ~@
               Serve events from all TIDELog files (\"tide\" file ~
               type) subdirectories of the /vol/studies/slots."
          program-name))
