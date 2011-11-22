;;; protocol.lisp --- Protocol functions for bag/channel/entry transcoding.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rsbag.tools.merge)

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
