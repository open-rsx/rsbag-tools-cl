;;;; info.lisp --- Implementation of the info command.
;;;;
;;;; Copyright (C) 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

(defclass info (file-input-mixin
                rsb.tools.commands:output-stream-mixin
                print-items:print-items-mixin)
  ((compute-sizes? :initarg  :compute-sizes?
                   :type     boolean
                   :reader   info-compute-sizes?
                   :initform nil
                   :documentation
                   "Compute the sizes of the content of the whole log
                    file and individual channels. This may take some
                    time for large files."
                   #+later (:short-name "s"))
   (print-format   :initarg  :print-format
                   :type     (member :no :short :full)
                   :reader   info-print-format
                   :initform :short
                   :documentation
                   "Print format information for each channel?."
                   #+later (:short-name    "f")))
  (:documentation
   "Display information about a log file.

    The file format of the log file is guessed based on the
    filename."))

(augment-class-documentation-with-backends 'info)

(service-provider:register-provider/class
 'command :info :class 'info)

(defun channel-size (channel)
  "Return the size of the content of CHANNEL in bytes."
  (reduce #'+ channel :key #'length))

(defmethod rsb.tools.commands:command-execute ((command info)
                                               &key error-policy)
  (declare (ignore error-policy))
  (let+ (((&accessors-r/o (input-files    command-input-files)
                          (stream         rsb.tools.commands:command-stream)
                          (compute-sizes? info-compute-sizes?)
                          (print-format   info-print-format))
          command)
         ((&flet process-bag (input)
            (with-bag (bag input :direction :input
                                 :transform '(nil))
              (format stream "File ~S~
                              ~&~2T~<~@;~@{~@(~8A~): ~
                              ~:[N/A~;~:*~,,',:D~]~^~&~}~:>~
                              ~&~2T~@<~@;~:{Channel ~
                              ~S~&~4T~@<~@;~{~@(~8A~): ~
                              ~:[N/A~;~:*~,,',:D~]~^~&~}~:>~&~}~:>~&"
                      input
                      (let+ (((&accessors-r/o start-timestamp end-timestamp) bag)
                             (duration (when (and start-timestamp end-timestamp)
                                         (local-time:timestamp-difference
                                          end-timestamp start-timestamp))))
                        `(:events ,(reduce #'+ (bag-channels bag) :key #'length)
                          ,@(when compute-sizes?
                              `(:size ,(reduce #'+ (bag-channels bag)
                                               :key #'channel-size)))
                          :start    ,start-timestamp
                          :end      ,end-timestamp
                          :duration ,duration))
                      (iter (for channel each (sort (copy-seq (bag-channels bag))
                                                    #'string<
                                                    :key #'channel-name))
                            (let+ (((&accessors-r/o
                                     length start-timestamp end-timestamp) channel)
                                   (duration (when (and start-timestamp end-timestamp)
                                               (local-time:timestamp-difference
                                                end-timestamp start-timestamp))))
                              (collect (list (channel-name channel)
                                             `(:type     ,(meta-data channel :type)
                                               :format   ,(when-let ((format (meta-data channel :format)))
                                                            (case print-format
                                                              (:short
                                                               (apply #'concatenate 'string
                                                                      (subseq format 0 (min 100 (length format)))
                                                                      (when (> (length format) 100)
                                                                        (list "…"))))
                                                              (:full
                                                               format)))
                                               :events   ,length
                                               ,@(when compute-sizes?
                                                   `(:size ,(channel-size channel)))
                                               :start    ,start-timestamp
                                               :end      ,end-timestamp
                                               :duration ,duration
                                               :rate     ,(when (and duration (plusp duration))
                                                            (/ length duration))))))))))))
    (mapc #'process-bag input-files)))

;; Local Variables:
;; coding: utf-8
;; End:
