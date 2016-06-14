;;;; info.lisp --- Implementation of the info command.
;;;;
;;;; Copyright (C) 2014, 2015, 2016 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

;;; `info-style' service

(service-provider:define-service info-style
  (:documentation
   "Providers of this service implement printing and other processing
    of bag information"))

;;; `bag-style-tree' class

(defclass bag-style-tree ()
  ((builder :initarg  :builder
            :reader   style-builder
            :documentation
            "Stores the builder that should be used to traverse the
             bag and its channels."))
  (:default-initargs
   :builder (missing-required-initarg 'bag-style-tree :builder))
  (:documentation
   "Format bag and channel info as a textual tree."))

(declaim (special *stream*))

(defmethod rsb.formatting:format-event ((event  bag)
                                        (style  bag-style-tree)
                                        (stream t)
                                        &key)
  (let+ (((&structure-r/o style- builder) style)
         ((&flet format* (format-control &rest format-arguments)
            (apply #'format *stream* format-control format-arguments)))
         ((&flet format-slot (name value
                              &key
                              (format   "~A")
                              (newline? t))
            (format *stream* "~@(~8A~): ~:[~2*N/A~;~?~]~:[~;~@:_~]"
                    name value format (list value) newline?)))
         ((&flet format-default-slots (&key
                                       event-count data-size
                                       start end duration rate
                                       &allow-other-keys)
            (format-slot :events   event-count :format "~,,',:D")
            (when data-size
              (format-slot :size data-size       :format "~,,',:D"))
            (format-slot :start    start)
            (format-slot :end      end)
            (format-slot :duration duration    :format "~,6F")
            (format-slot :rate     rate        :format "~,3F" :newline? nil)))
         ((&flet format-bag (recurse
                             &rest initargs &key location &allow-other-keys)
            (format* "File ~S~@:_~2@T" location)
            (pprint-logical-block (*stream* nil)
              (apply #'format-default-slots initargs)
              (pprint-newline :mandatory *stream*)
              (funcall recurse))))
         ((&flet format-channel (recurse
                                 &rest initargs &key name &allow-other-keys)
            (format* "Channel ~S~@:_~2@T" name)
            (pprint-logical-block (*stream* nil)
              (funcall recurse)
              (apply #'format-default-slots initargs))
            (pprint-newline :mandatory *stream*)))
         ((&flet format-node (recurse relation relation-args
                              node kind relations &rest initargs)
            (declare (ignore relation relation-args node relations))
            (apply (ecase kind
                     (rsbag:bag     #'format-bag)
                     (rsbag:channel #'format-channel))
                   recurse initargs)))
         ((&flet peek (builder relation relation-args node)
            (declare (ignore builder relation-args))
            (case relation
              ((:type :format)
               (format-slot relation (list node) :format "~<~@;~A~:>"))
              (t
               t))))
         (*stream* stream))
    (pprint-logical-block (*stream* (list event))
      (architecture.builder-protocol:with-unbuilder (builder builder)
        (architecture.builder-protocol:walk-nodes
         builder
         (architecture.builder-protocol:peeking #'peek #'format-node)
         event)))
    (fresh-line stream)))

(service-provider:register-provider/class
 'info-style :tree :class 'bag-style-tree)

;;; `info' command class

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
                   :type     (member nil :short :full)
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

(defmethod rsb.tools.commands:command-execute ((command info)
                                               &key error-policy)
  (declare (ignore error-policy))
  (let+ (((&accessors-r/o (input-files    command-input-files)
                          (stream         rsb.tools.commands:command-stream)
                          (compute-sizes? info-compute-sizes?)
                          (print-format   info-print-format))
          command)
         (builder (make-instance 'rsbag.builder:unbuilder
                                 :compute-sizes? compute-sizes?
                                 :format?        print-format))
         (style   (rsb.formatting:make-style :tree
                                             :builder builder
                                             :service 'info-style))
         ((&flet process-bag (input)
            (with-bag (bag input :direction :input :transform '(nil))
              (rsb.formatting:format-event bag style stream)))))
    (mapc #'process-bag input-files)))
