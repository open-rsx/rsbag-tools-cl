;;;; protocol.lisp --- Protocol functions for bag/channel/entry transcoding.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package :rsbag.tools.merge)

(defgeneric make-channel-name (input-channel)
  (:documentation
   "Return a suitable name for the output channel into which entries
from INPUT-CHANNEL will be copied."))

(defgeneric make-channel-type (input-channel)
  (:documentation
   "Return a suitable type for the output channel into which entries
from INPUT-CHANNEL will be copied."))

(defgeneric make-channel-meta-data (input-channel)
  (:documentation
   "Return a suitable meta-data plist for the output channel into
which entries from INPUT-CHANNEL will be copied."))

(defgeneric transform-timestamp (timestamp entry)
  (:documentation
   "Return a (possibly) transformed version of TIMESTAMP which is
associated to ENTRY, suitable for associating ENTRY to it."))

(defgeneric transform-entry (timestamp entry)
  (:documentation
   "Return a (possibly) transformed version of ENTRY for storage in an
output channel."))

(defgeneric transcode (input output
		       &key
		       channels
		       skip-empty-channels?
		       progress)
  (:documentation
   "Copy and potentially transform the contents of INPUT to
OUTPUT. INPUT and OUTPUT can be bags or channels or sequences of such.

When INPUT and OUTPUT are bags (or sequences of bags) and CHANNELS is
supplied, it is used to select channels from INPUT from
processing. When supplied, CHANNELS has be either t or a unary
predicate.

When INPUT and OUTPUT are bags (or sequences of bags)
SKIP-EMPTY-CHANNELS? can be used to control whether empty channels
should be created in the output bag for empty input channels.

If PROGRESS is non-nil it has to be a function accepting six
arguments: a current index, an overall length, an input bag, an input
channel, an output bag and an output channel. "))


;;; Default behavior
;;

(defmethod transform-timestamp ((timestamp t) (entry t))
  "Default behavior is to not modify TIMESTAMP."
  timestamp)

(defmethod transform-entry ((timestamp t) (entry t))
  "Default behavior is to not modify ENTRY."
  entry)
