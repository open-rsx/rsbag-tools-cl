;;; help.lisp --- Help text generation for the bag-play program.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsbag.tools.play)

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
	    (map 'list #'car (rsbag.backend:backend-classes))
	    "bag-play")))

(defun make-replay-strategy-help-string (&key
					 (show :default))
  "Return a help string that explains how to specify replay strategies
and lists the available replay strategies."
  (with-output-to-string (stream)
    (format stream "Replay events form the specified input file ~
according to SPEC which has to be of the form

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values depend on KIND and are optional in most ~
cases. Examples (note that the single quotes have to be included only ~
when used within a shell):

  --replay-strategy recorded-timing
  -r as-fast-as-possible
  --replay-strategy 'fixed-rate :rate 10'
  -r 'remote-controlled :uri \"spread://localhost:4803/myplayback/control\"'

")
    (with-abbreviation (stream :strategies show)
      (format stream "Currently, the following strategies are supported:

" )
      (print-classes-help-string
       (replay-strategy-classes) stream
       :initarg-blacklist '(:start-index :end-index :error-policy
			    :stream :previous-command)))))

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
