;;;; help.lisp --- Help text generation for the bag-play program.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.play)

(defun make-help-string (&key
			 (show :default))
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Replay events from INPUT-FILE on RSB channels ~
derived from BASE-URI.

")
    (with-abbreviation (stream :uri show)
      (format stream "BASE-URI which has to be of the form

  ")
      (print-uri-help stream :uri-var "BASE-URI"))
    (format stream "

The file format of INPUT-FILE is guessed based on the ~
filename. Currently, the following file formats are supported:~{~&+ ~
~4A (extension: \".~(~:*~A~)\")~}"
	    (mapcar #'car (rsbag.backend:backend-classes))
	    "bag-play")))

(defun make-examples-string (&key
			     (program-name "bag-play"))
  "Make and return a string containing usage examples of the program."
  (format nil "~A /tmp/everything.tide spread://azurit:4803/

  Replay all events from all channels stored in the log file ~
\"/tmp/everything.tide\" mimicking the recorded timing as closely as ~
possible. Publish replayed events on the channels they were originally ~
published on. Use the Spread daemon listening on port 4803 of host ~
azurit to connect to the bus.

~:*~A -r as-fast-as-possible -c /nao/vision/top /tmp/nao.tide ~
'spread:/mynamespace'

  Replay events recorded from the log file \"/tmp/nao.tide\" for the ~
channel designated by \"/nao/vision/top\" (and potentially recorded ~
sub-channels) as fast as possible, discarding the recorded timing ~
information. Publish replayed on channels with the prefix ~
\"/mynamespace\", i.e. \"/mynamespace/nao/vision/top\" for events ~
recorded for \"/nao/vision/top\". "
	  program-name))
