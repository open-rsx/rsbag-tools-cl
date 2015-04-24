;;;; mixins.lisp --- Mixin classes used by the RSBag command classes.
;;;;
;;;; Copyright (C) 2013, 2014, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.commands)

;;; `progress-mixin'

(defclass progress-mixin () ; TODO should not be a command mixin
  ((progress-style :initarg  :progress-style
                   :type     symbol
                   :reader   command-progress-style
                   :initform :line
                   :documentation
                   "Indicate progress of the ongoing playback using
                    style STYLE."
                   #+later (:short-name "p"
                            :argument-name "STYLE")))
  (:documentation
   "This class is intended to be mixed into command classes that
    display the progress of an operation."))

;;; File-related stuff

(defun augment-documentation-with-backends (next-documentation)
  (format nil "~@<~A Currently, the following file formats are supported:~@
               ~:@_~
               ~{+ ~4A (extension: \".~(~:*~A~)\")~^~@:_~}~@:>"
          next-documentation
          (mapcar #'car (rsbag.backend:backend-classes))))

(defun augment-class-documentation-with-backends (class-name)
  (let ((class (find-class class-name)))
    (defmethod documentation ((thing (eql class)) (doc-type (eql t)))
      (augment-documentation-with-backends (call-next-method)))))

(defun augment-slot-documentation-with-backends (class-name slot-name)
  (let* ((class (find-class class-name))
         (slot  (progn
                  (c2mop:finalize-inheritance class)
                  (find slot-name (c2mop:class-direct-slots class)
                        :key #'c2mop:slot-definition-name))))
    (defmethod documentation ((thing (eql slot)) (doc-type (eql t)))
      (augment-documentation-with-backends (call-next-method)))))

(defun collect-input-files (spec)
  "Collect and return a list of input files according to SPEC.

   SPEC can be:
   + A designator of a wild pathname matching one or more files
   + A list of pathname designators of existing files"
  (let+ ((parsed nil)
         ((&flet parsed ()
            (or parsed
                (setf parsed (mapcar #'parse-namestring (ensure-list spec))))))
         ((&flet existing-file (pathname)
            (when-let ((probed (probe-file pathname)))
              (and (pathname-name probed) (pathname-type probed))))))
        (cond
          ;; Neither glob expression nor filenames
          ((null spec)
           (error "~@<No glob expression or one or more input files ~
                   specified.~@:>"))

          ;; A single glob expression. Does it match anything?
          ((and (length= 1 (parsed)) (wild-pathname-p (first (parsed))))
           (or (directory (first (parsed)))
               (error "~@<The specified input glob expression ~S did ~
                       not match any files.~@:>"
                      (first (parsed)))))

          ;; Multiple arguments: should refer to existing files.
          ((when-let ((invalid (remove-if #'existing-file (parsed))))
             (error "~@<The following specified input file~P~:* ~
                     ~[~;does~:;do~] not exist: ~{~S~^, ~}.~@:>"
                    (length invalid) invalid)))

          ;; We don't understand anything else.
          (t
           (parsed)))))

;;; `file-output-mixin'

(eval-when (:compile-toplevel  :load-toplevel :execute)
  (defclass file-output-mixin ()
    ((output-file :type     (or null pathname)
                  :reader   command-output-file
                  :accessor command-%output-file
                  :documentation
                  "Name of the file into which captured events should
                   be written. The file format is determined based on
                   the file type (extension)."
                   #+later (:short-name "o"))
     (force?      :initarg   :force?
                  :type      boolean
                  :reader    command-force?
                  :initform  nil
                  :documentation
                  "Should the output file be overwritten in case it
                   already exists?"))
    (:default-initargs
     :output-file (missing-required-initarg 'file-output-mixin :output-file))
    (:documentation
     "This class is intended to be mixed into command classes that
      write output data into a file.")))

(augment-slot-documentation-with-backends 'file-output-mixin 'output-file)

(defmethod shared-initialize :after ((instance   file-output-mixin)
                                     (slot-names t)
                                     &key
                                     (output-file nil output-file-supplied?))
  (when output-file-supplied?
    (setf (command-%output-file instance)
          (when output-file (pathname output-file)))))

(defmethod print-items:print-items append ((object file-output-mixin))
  `((:output-file-marker nil                           " => ")
    (:output-file        ,(command-output-file object) "~A"
     ((:after :output-file-marker)))))

;;; `file-input-mixin'

(eval-when (:compile-toplevel  :load-toplevel :execute)
  (defclass file-input-mixin ()
    ((input-files :type     (or null (cons pathname)) ; list-of pathnanmes
                  :accessor command-%input-files
                  :documentation
                  "Stores a list of pathnames of input files."))
    (:default-initargs
     :input-files (missing-required-initarg 'file-input-mixin :input-files))
    (:documentation
     "This class is intended to be mixed into command classes that
      read input data from files.")))

(augment-slot-documentation-with-backends 'file-input-mixin 'input-files)

(defmethod shared-initialize :after ((instance   file-input-mixin)
                                     (slot-names t)
                                     &key
                                     (input-files nil input-files-supplied?))
  (when input-files-supplied?
    (setf (command-%input-files instance) (mapcar #'pathname input-files))))

(defmethod command-input-files ((command file-input-mixin))
  (collect-input-files (command-%input-files command)))

(defmethod print-items:print-items append ((object file-input-mixin))
  `((:input-file ,(command-input-files object) "~{~A~^, ~}"
     ((:before :output-file-marker)))))

;;; `index-timestamp-mixin'

(defclass index-timestamp-mixin ()
  ((index-timestamp :initarg  :index-timestamp
                    :type     keyword
                    :reader   command-index-timestamp
                    :initform :send
                    :documentation
                    "Name of the timestamp which should be used to
                     index events in the created log file."))
  (:documentation
   "This class is intended to be mixed into command classes that store
    and index events based on a configurable timestamp."))

;;; `replay-mixin'

(defclass replay-mixin ()
  ((channels        :initarg  :channels
                    :type     (or (eql t) function)
                    :reader   command-replay-channels
                    :initform t
                    :documentation
                    "Select the channels matching NAME-OR-REGEXP for
                     replay. This option can be specified multiple
                     times."
                    #+later ( :short-name "c"
                              :argument-name "NAME-OR-REGEXP"))
   (start-time      :initarg  :start-time
                    :type     rsbag.rsb.replay:range-boundary/timestamp
                    :reader   command-replay-start-time
                    :initform nil
                    :documentation
                    "Start replaying entries at the point in time
                     indicated by TIMESTAMP-OR-SECONDS.

                     When the value should be parsed as a timestamp,
                     the syntax @[YYYY-MM-DDT]HH:MM:SS has to be used.

                     A single positive real number is interpreted as
                     time in seconds relative to the beginning of the
                     replay. Similarly, a single negative real number
                     is interpreted as time in seconds relative to the
                     end of the replay, e.g. -2.5 indicates \"2.5
                     seconds before the end of the replay\".

                     Mutually exclusive with start-index."
                    #+later (:short-name    "s"
                             :argument-name "TIMESTAMP-OR-SECONDS"))
   (start-index     :initarg  :start-index
                    :type     (or null integer)
                    :reader   command-replay-start-index
                    :initform nil
                    :documentation
                    "Index of the entry at which the replay should start.

                     A non-negative integer N is interpreted as
                     the (N+1)-th entry relative to the beginning of
                     the replay, i.e. 0 designates the first entry. A
                     negative integer N is interpreted as |N| entries
                     back from the end of the replay.

                     Mutually exclusive with start-time."
                    #+later (:short-name    "S"
                             :argument-name "INDEX"))
   (end-time        :initarg  :end-time
                    :type     rsbag.rsb.replay:range-boundary/timestamp
                    :reader   command-replay-end-time
                    :initform nil
                    :documentation
                    "Stop replaying entries at the point in time
                     indicated by TIMESTAMP-OR-SECONDS.

                     When the value should be parsed as a timestamp,
                     the syntax @[YYYY-MM-DDT]HH:MM:SS has to be used.

                     A single real number is interpreted as time in
                     seconds relative to the beginning of the
                     replay. Similarly, a single negative real number
                     is interpreted as time in seconds relative to the
                     end of the replay, e.g. -2.5 indicates \"2.5
                     seconds before the end of the replay\".

                     Mutually exclusive with end-index."
                    #+later (:short-name    "e"
                             :argument-name "TIMESTAMP-OR-SECONDS"))
   (end-index       :initarg  :end-index
                    :type     (or null integer)
                    :reader   command-replay-end-index
                    :initform nil
                    :documentation
                    "Index of the entry at which the replay should end.

                     A non-negative integer N is interpreted as
                     the (N+1)-th entry relative to the beginning of
                     the replay, i.e. 0 designates the first entry. A
                     negative integer N is interpreted as |N| entries
                     back from the end of the replay.

                     Mutually exclusive with end-time."
                    #+later (:short-name    "E"
                             :argument-name "INDEX"))
   (num-repetitions :initarg  :num-repetitions
                    :type     (or (eql t) positive-integer)
                    :reader   command-replay-num-repetitions
                    :initform 1
                    :documentation
                    "Number of times the selected should be processed.")
   (replay-strategy :initarg  :replay-strategy
                    :reader   command-replay-strategy
                    :accessor command-%replay-strategy
                    :documentation
                    "The replay strategy that should be used for
                     processing the selected entries."
                     #+later (make-replay-strategy-help-string :show show)
                     #+later (:short-name "r"
                              :argument-name "SPEC"))))

(defmethod shared-initialize :before
    ((instance   replay-mixin)
     (slot-names t)
     &key
     (replay-strategy      nil replay-strategy-supplied?)
     (replay-strategy-spec nil replay-strategy-spec-supplied?))
  (cond
    ((and replay-strategy-supplied? replay-strategy-spec-supplied?)
     (incompatible-initargs 'replay-mixin
                            :replay-strategy      replay-strategy
                            :replay-strategy-spec replay-strategy-spec))
    ((not (or replay-strategy-supplied? replay-strategy-spec-supplied?))
     (missing-required-initarg 'replay-mixin
                               :replay-strategy-xor-replay-strategy-spec))))

(defmethod shared-initialize :after
    ((instance   replay-mixin)
     (slot-names t)
     &key
     (replay-strategy-spec nil replay-strategy-spec-supplied?))
  (when replay-strategy-spec-supplied?
    (setf (command-%replay-strategy instance)
          (rsbag.rsb:make-replay-strategy
           (parse-instantiation-spec replay-strategy-spec)))))

(defun command-replay-bounds (command)
  (let+ (((&structure-r/o
           command-replay- start-time start-index end-time end-index)
          command))
    ;; Check mutually exclusive options.
    (when (and start-time start-index)
      (error "~@<The commandline options \"start-time\" and ~
              \"start-index\" are mutually exclusive.~@:>"))
    (when (and end-time end-index)
      (error "~@<The commandline options \"end-time\" and ~
              \"end-index\" are mutually exclusive.~@:>"))

    (values start-time start-index end-time end-index)))
