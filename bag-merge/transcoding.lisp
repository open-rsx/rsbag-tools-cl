;;; transcoding.lisp --- Transcoding input bags into an output bag.
;;
;; Copyright (C) 2011, 2012 Jan Moringen
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

(defmethod make-channel-name ((input-channel t))
  (channel-name input-channel))

(defmethod make-channel-type ((input-channel t))
  (getf (channel-meta-data input-channel) :type))

(defmethod make-channel-meta-data ((input-channel t))
  (let ((meta-data (channel-meta-data input-channel)))
    (append (list :type (make-channel-type input-channel))
	    (remove-from-plist meta-data :type))))

(defmethod transcode ((input channel) (output channel)
		      &key
		      progress
		      &allow-other-keys)
  (bind ((length (length input))
	 ((:flet update-progress (i))
	  (cond
	    ((not progress))
	    ((eq i t)
	     (funcall progress t))
	    ((or (zerop (mod i 100)) (= i (1- length)))
	     (funcall progress i length
		      (rsbag:channel-bag input) input
		      (rsbag:channel-bag output) output)))))
    (iter (for (timestamp datum) each (channel-items input))
	  (for i :from 0)
	  (setf (entry output timestamp) datum)
	  (update-progress i))
    (update-progress t)))

(defmethod transcode ((input bag) (output bag)
		      &rest args
		      &key
		      channels
		      skip-empty-channels?
		      &allow-other-keys)
  (bind (((:flet skip-channel? (channel))
	  (or (and skip-empty-channels? (emptyp channel))
	      (and (not (eq channels t))
		   (not (funcall channels channel)))))
	 ((:flet clone-channel (source))
	  (bind ((name      (make-channel-name source))
		 (meta-data (make-channel-meta-data source)))
	    (or (bag-channel output name :if-does-not-exist nil)
		(setf (bag-channel output name) meta-data))))
	 (in-channels  (remove-if #'skip-channel? (bag-channels input)))
	 (out-channels (map 'list #'clone-channel in-channels))
	 (other-args   (remove-from-plist
			args :channels :skip-empty-channels?)))
    (iter (for in  each in-channels)
	  (for out each out-channels)
	  (apply #'transcode in out other-args))))

(defmethod transcode ((input sequence) (output bag)
		      &rest args &key
		      &allow-other-keys)
  (map nil #'(lambda (bag) (apply #'transcode bag output args)) input))
