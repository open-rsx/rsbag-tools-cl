;;;; transform.lisp --- Implementation of the transform command.
;;;;
;;;; Copyright (C) 2013, 2014, 2015, 2016, 2017 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

;;; `transform' command class

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass transform (file-input-mixin
                       file-output-mixin
                       bag->events-mixin
                       progress-mixin
                       print-items:print-items-mixin)
    ((transform/datum     :initarg  :transform/datum
                          :reader   command-transform/datum
                          :initform nil
                          :documentation
                          "If not nil, a function designator for
                           transforming entries. The function is
                           called with an entry and must return the
                           transformed entry. ")
     (transform/timestamp :initarg  :transform/timestamp
                          :reader   command-transform/timestamp
                          :initform nil
                          :documentation
                          "If not nil, a function designator for
                           transforming timstamps. The function is
                           called with the original timestamp and
                           the (potentially already transformed) entry
                           and must return the transformed
                           timestamp."))
    (:documentation
     "Copy events from input log files into a new log file.

      Copy entries from input files matching GLOB-EXPRESSION or from an
      explicitly given list of INPUT-FILEs into the specified output
      file. In addition to the canonical globbing syntax, expressions of
      the form

        SOMESTUFF/**/MORESTUFF

      can be used to search directories recursively.

      The file formats of input files and the output file are determined
      based on the file type (extension).")))

(augment-class-documentation-with-backends 'transform)

(service-provider:register-provider/class
 'command :transform :class 'transform)

(defmethod rsb.tools.commands:command-execute ((command transform)
                                               &key error-policy)
  (let+ (((&accessors-r/o (input-files         command-input-files)
                          (channels            command-replay-channels)
                          ((&values start-time start-index end-time end-index)
                           command-replay-bounds)
                          (output-file         command-output-file)
                          (force?              command-force?)
                          (transform/datum     command-transform/datum)
                          (transform/timestamp command-transform/timestamp)
                          (progress-style      command-progress-style))
          command)
         ;; Plumbing starts here.
         (sink)
         ((&flet process-datum (timestamp datum)
            (let+ ((datum/transformed     (if transform/datum
                                              (funcall transform/datum datum)
                                              datum))
                   (timestamp/transformed (if transform/timestamp
                                              (funcall transform/timestamp
                                                       timestamp datum/transformed)
                                              timestamp)))
              (when datum/transformed
                (rsbag.rsb.recording:process-event
                 sink timestamp/transformed datum/transformed)))))
         (channel-allocation (make-instance 'clone))
         ((&flet wrap-process-datum-for-clone ()
            (let+ (((&flet note-source (source timestamp event)
                      (declare (ignore timestamp))
                      (push (cons event source)
                            (strategy-%events channel-allocation)))))
              (note-source-channel #'process-datum #'note-source))))
         (sink-function (wrap-process-datum-for-clone))

         (access         (%access transform/datum channel-allocation))
         (make-transform (ecase access
                           (:data  (lambda () (coding-transform t)))
                           (:event (lambda () (coding-transform nil)))
                           ((nil)  (constantly '(nil))))))
    (rsbag.rsb:with-open-connection
        (input (apply #'rsbag.rsb:bag->events
                      input-files sink-function
                      :error-policy    error-policy
                      :channels        channels
                      :transform       (funcall make-transform)
                      :replay-strategy :as-fast-as-possible
                      (append (when start-time
                                (list :start-time start-time))
                              (when start-index
                                (list :start-index start-index))
                              (when end-time
                                (list :end-time end-time))
                              (when end-index
                                (list :end-index end-index)))))
      (rsbag.rsb:with-open-connection
          (output (rsbag.rsb:events->bag
                   nil output-file
                   :error-policy     error-policy
                   :if-exists        (if force? :supersede :error)
                   :transform        (funcall make-transform)
                   :channel-strategy channel-allocation))
        (setf sink output)
        (rsbag.rsb.replay:replay
         input (rsbag.rsb.replay:connection-strategy input)
         :progress (case progress-style
                     (:line  (curry #'print-replay-progress *info-output*))
                     (:ready (curry #'print-ready *info-output*))))))))

;;; Determine required amount of de/encoding

(defun %access (transform? channel-strategy)
  (let+ ((processors channel-strategy)
         ((&flet parts ()
            (mapcar #'car rsb.event-processing:*event-parts*))))
    (cond
      ((or transform? (rsb.ep:access? processors :data :read))
       :data)
      ((rsb.ep:access? processors (parts) :read)
       :event)
      (t
       nil))))

;;; `clone' channel allocation strategy
;;;
;;; In the `channels' slot, the strategy instance stores a mapping
;;; from channels in the source bag(s) to channels in the destination
;;; bag. This mapping is initially empty when the strategy object is
;;; created.
;;;
;;; When an event is processed, a cell of the form
;;;
;;;   (EVENT . SOURCE-CHANNEL)
;;;
;;; must be pushed onto the alist stored in the `events' slot. This is
;;; used to look up SOURCE-CHANNEL when EVENT is processed in the
;;; `ensure-channel-for' method.

(defclass clone ()
  ((channels :reader   strategy-channels
             :initform (make-hash-table :test #'eq)
             :documentation
             "Maps channels in the source bag(s) to channels in the
              destination bag.")
   (events   :accessor strategy-%events
             :initform '()
             :documentation
             "An always almost empty alist of elements of the form

                (EVENT . SOURCE-CHANNEL)

              ."))
  (:documentation
   "Clones channels in the source into the destination."))

(defmethod rsbag.rsb.recording:ensure-channel-for ((connection t)
                                                   (event      t)
                                                   (strategy   clone))
  (let+ (((&structure strategy- channels %events) strategy)
         (source-channel (prog1
                             (assoc-value %events event :test #'eq)
                           (removef %events event :test #'eq :key #'car)))
         (found? t)
         ((&flet make-channel (condition)
            (declare (ignore condition))
            (setf found? nil)
            (let ((meta-data (rsbag:channel-meta-data source-channel)))
              (invoke-restart 'rsbag:create meta-data))))
         ((&flet maybe-make-channel ()
            (let ((bag  (rsbag.rsb:connection-bag connection))
                  (name (rsbag:channel-name source-channel)))
              (rsbag:bag-channel
               bag name :if-does-not-exist #'make-channel)))))
    (values (ensure-gethash source-channel channels (maybe-make-channel))
            found?)))

;;; `note-source-channel'
;;;
;;; Helper construct which wraps a function for recording source
;;; channels of events around the function provided to `bag->events'.

(defstruct (note-source-channel
             (:constructor note-source-channel (function note-function))
             (:predicate nil)
             (:copier nil))
  (function      nil :type function :read-only t)
  (note-function nil :type function :read-only t))

(defmethod rsbag.rsb:bag->events ((source channel) (dest note-source-channel)
                                  &rest args &key)
  (let+ (((&structure-r/o note-source-channel- function note-function) dest)
         ((&flet note-channel (timestamp event)
            (funcall note-function source timestamp event)
            (funcall function timestamp event))))
    (apply #'rsbag.rsb:bag->events source #'note-channel args)))
