;;;; help.lisp --- Help text generation for the bag-record program.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.record)

(defun make-help-string (&key
			 (show :default))
  "Return a help that explains the commandline option interface."
  (with-output-to-string (stream)
    (format stream "Capture events being exchanged on the scopes ~
designated by URIS and store them in the specified output file. For ~
each URI, one or more channels may be created in the output ~
file (depending on the channel allocation strategy). These channels ~
store the events exchanged in the corresponding RSB channels.

")
    (with-abbreviation (stream :uri show)
      (format stream "Each URI has to be of the form

  ")
      (print-uri-help stream :uri-var "each URI"))))

(defun make-channel-strategy-help-string (&key
					  (show :default))
  "Return a help string that explains how to specify channel
allocation strategies and lists the available strategies."
  (with-output-to-string (stream)
    (format stream "Allocate channels for received events in the ~
output bag file according to the strategy designated by SPEC which has ~
to be of the form

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values depend on KIND and are optional in most ~
cases. Examples (note that the single quotes have to be included only ~
when used within a shell):

  -a scope-and-type

")
    (with-abbreviation (stream :channel-strategies show)
      (format stream "Currently, the following strategies are supported:

" )
      (print-classes-help-string (channel-strategy-classes) stream))))

(defun make-filter-help-string (&key
				(show :default))
  "Return a help string that explains how to specify filters and lists
the available filters. "
  (with-output-to-string (stream)
    (format stream "Specify a filter that received events have to ~
match in order to be processed rather than discarded. This option can ~
be supplied multiple times in which case events have to match all ~
specified filters. Each SPEC has to be of the form

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values depend on KIND and may be optional in some ~
cases. Examples (note that the single quotes have to be included only ~
when used within a shell):

  --filter 'origin \"EAEE2B00-AF4B-11E0-8930-001AA0342D7D\"'
  --filter 'regex \".*foo[0-9]+\"'
  --filter 'regex :regex \".*foo[0-9]+\"' (equivalent)
  -f 'xpath :xpath \"node()/@foo\" :fallback-policy :do-not-match'

")
    (with-abbreviation (stream :filters show)
      (format stream "The following filters are currently available:

")
      (print-filter-help stream))))

(defun make-flush-strategy-help-string (&key
					(show :default))
  "Return a help string that explains how to specify flush strategies
and lists the available strategies."
  (with-output-to-string (stream)
    (format stream "Flush buffers to background storage according to ~
the strategy designated by SPEC which has to be of the form ~

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values depend on KIND and are optional in most ~
cases. Examples (note that the single quotes have to be included only ~
when used within a shell):

  --flush-strategy 'property-limit :property :length/bytes :limit 33554432'
  --flush-strategy 'property-limit :property :time-to-last-write :limit 5'
  --flush-strategy 'or (:property-limit :property :time-to-last-write :limit 5)
                       (:property-limit :property :length/bytes :limit 33554432)'


")
    (with-abbreviation (stream :flush-strategies show)
      (format stream "Currently, the following strategies are supported:

" )
      (print-classes-help-string (rsbag.backend:flush-strategy-classes) stream))))

(defun make-examples-string (&key
			     (program-name "bag-record"))
  "Make and return a string containing usage examples of the program."
  (format nil "~A -o /tmp/everything.tide spread://azurit:4803/

  Connect to the bus using the Spread transport assuming the Spread ~
daemon is listening on port 4803 on host \"azurit\". Participate in ~
the \"root\" channel (designated by \"/\") and capture all events into ~
the log file \"/tmp/everything.tide\". Recording continues until the ~
program receives SIGINT or SIGTERM each of which causes a clean ~
shutdown.

~:*~A -o /tmp/nao.tide -f 'xpath :xpath \"node()/@width = ~
100\" :fallback-policy :do-not-match' ~
'spread:/nao/vision/top?name=4803'

  Store events for the scope \"/nao/vision/top\" (and sub-scopes) in ~
the log file \"/tmp/nao.tide\", if the event payload has a width field ~
that has the value 100. The bus connection uses the Spread transport ~
with the daemon name option.

~:*~A -o /tmp/multichannel.tide 'spread:/nao/vision/top' ~
'spread:/nao/audio/all'

  Store events being exchanged on the channels designated by ~
\"/nao/vision/top\" and \"/nao/audio/all\" into the log file ~
\"/tmp/multichannel.tide\".
"
	  program-name))
