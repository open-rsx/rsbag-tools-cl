;;;; transcoding.lisp --- Transcoding input bags into an output bag.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.merge)

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
                      (transform           #'identity)
                      (transform/timestamp (lambda (timestamp datum)
                                             (declare (ignore datum))
                                             timestamp))
                      &allow-other-keys)
  (let+ ((length (length input))
         ((&flet update-progress (i)
            (cond
             ((not progress))
             ((eq i t)
              (funcall progress t))
             ((or (zerop (mod i 100)) (= i (1- length)))
              (funcall progress i length
                       (rsbag:channel-bag input) input
                       (rsbag:channel-bag output) output))))))
    (iter (for (timestamp datum) each (channel-items input))
          (for i :from 0)
          (let* ((datum/transformed     (funcall transform datum))
                 (timestamp/transformed (funcall transform/timestamp
                                                 timestamp datum/transformed)))
            (setf (entry output timestamp/transformed) datum/transformed))
          (update-progress i))
    (update-progress t)))

(defmethod transcode ((input bag) (output bag)
                      &rest args
                      &key
                      channels
                      skip-empty-channels?
                      &allow-other-keys)
  (let+ (((&flet skip-channel? (channel)
            (or (and skip-empty-channels? (emptyp channel))
                (and (not (eq channels t))
                     (not (funcall channels channel))))))
         ((&flet clone-channel (source)
            (let ((name      (make-channel-name source))
                  (meta-data (make-channel-meta-data source)))
              (or (bag-channel output name :if-does-not-exist nil)
                  (setf (bag-channel output name) meta-data)))))
         (in-channels  (remove-if #'skip-channel? (bag-channels input)))
         (out-channels (mapcar #'clone-channel in-channels))
         (other-args   (remove-from-plist
                        args :channels :skip-empty-channels?)))
    (iter (for in  each in-channels)
          (for out each out-channels)
          (apply #'transcode in out other-args))))

(defmethod transcode ((input sequence) (output bag)
                      &rest args &key
                      &allow-other-keys)
  (map nil (lambda (bag) (apply #'transcode bag output args)) input))
