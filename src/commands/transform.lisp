;;;; transform.lisp --- Implementation of the transform command.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

;;; Transcoding protocol

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

(defgeneric transcode (input output
                       &key
                       channels
                       skip-empty-channels?
                       entry-transform
                       timestamp-transform
                       progress)
  (:documentation
   "Copy and potentially transform the contents of INPUT to
    OUTPUT. INPUT and OUTPUT can be bags or channels or sequences of
    such.

    When INPUT and OUTPUT are bags (or sequences of bags) and CHANNELS
    is supplied, it is used to select channels from INPUT from
    processing. When supplied, CHANNELS has be either t or a unary
    predicate.

    When INPUT and OUTPUT are bags (or sequences of bags)
    SKIP-EMPTY-CHANNELS? can be used to control whether empty channels
    should be created in the output bag for empty input channels.

    If PROGRESS is non-nil it has to be a function accepting six
    arguments: a current index, an overall length, an input bag, an
    input channel, an output bag and an output channel. "))

;;; Transcoding implementation

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
                      (entry-transform     #'identity)
                      (timestamp-transform (lambda (timestamp datum)
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
          (let* ((datum/transformed     (funcall entry-transform datum))
                 (timestamp/transformed (funcall timestamp-transform
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

;;; `transform' command class

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defclass transform (file-input-mixin
                       file-output-mixin
                       replay-mixin
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
    (:default-initargs
     :replay-strategy (rsbag.rsb:make-replay-strategy :as-fast-as-possible))
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

(defun print-transform-progress (stream index
                                 &optional
                                 length
                                 input-bag  input-channel
                                 output-bag output-channel)
  (case index
    ((t)
     (fresh-line stream))
    (t
     (format stream "~C[~28A|~48A] -> [~28A|~48A] ~6,2,2F % ~9:D/~9:D"
             #\Return
             input-bag  (channel-name input-channel)
             output-bag (channel-name output-channel)
             (/ (1+ index) (max 1 length)) index (1- length))
     (force-output stream))))

(defmethod rsb.tools.commands:command-execute ((command transform)
                                               &key error-policy)
  (declare (ignore error-policy))
  (let+ (((&accessors-r/o (input               command-input-files)
                          (channels            command-replay-channels)
                          (output-file         command-output-file)
                          (force?              command-force?)
                          (transform/datum     command-transform/datum)
                          (transform/timestamp command-transform/timestamp)
                          (progress-style      command-progress-style))
          command)
         (inputs '())
         (output nil))
    ;; Since `with-bag' only handles one bag, we have to handle the
    ;; possibility of unwinding with multiple open bags manually.
    (unwind-protect
         (progn
           ;; Open files and store resulting bags successively to
           ;; ensure that open bags can be closed when unwinding.
           (iter (for file in input)
                 (format *info-output* "Opening input file ~S~%" file)
                 (push (open-bag file :direction :input) inputs))
           (format *info-output* "Opening output file ~S~%" output-file)
           (setf output (open-bag output-file
                                  :direction :output
                                  :if-exists (if force? :supersede :error)))

           ;; Transcode individual input files into output file.
           (apply #'transcode inputs output
                  :channels channels
                  :progress (case progress-style
                              (:line (curry #'print-transform-progress *info-output*)))
                  (append
                   (when transform/datum
                     (list :entry-transform transform/datum))
                   (when transform/timestamp
                     (list :timestamp-transform transform/timestamp)))))

      (iter (for bag in (list* output inputs))
            (when bag
              (handler-case
                  (close bag)
                (error (condition)
                  (warn "~@<Error closing bag ~A: ~A~@:>"
                        bag condition))))))))
