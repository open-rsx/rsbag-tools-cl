;;;; main.lisp --- Main function of the bag-info program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.info)

(defun make-help-string (&key
                         (program-name "bag-info"))
  "Return a help that explains the commandline option interface."
  (format nil "Display information about BAG-FILE.~@
               ~@
               The file format of BAG-FILE is guessed based on the ~
               filename. Currently, the following file formats are ~
               supported:~{~&+ ~4A (extension: \".~(~:*~A~)\")~}~@
               ~@
               Examples:~@
               ~@
               ~2@T~A /tmp/everything.tide~@
               "
          (mapcar #'car (rsbag.backend:backend-classes))
          program-name))

(defun update-synopsis (&key
                        (show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "BAG-FILE"
   :item    (make-text :contents (make-help-string))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Display Options")
              (flag :long-name  "compute-sizes"
                    :short-name "s"
                    :description
                    "Compute the sizes of the content of the whole log file and individual channels. This may take some time for large files.")
              (enum :long-name     "print-format"
                    :short-name    "f"
                    :enum          '(:no :short :full)
                    :default-value :short
                    :description
                    "Print format information for each channel."))))

(defun channel-size (channel)
  "Return the size of the content of CHANNEL in bytes."
  (reduce #'+ channel :key #'length))

(defun main ()
  "Entry point function of the bag-info program."
  (update-synopsis)
  (process-commandline-options
   :version         (cl-rsbag-tools-info-system:version/list :commit? t)
   :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
                          :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
   :update-synopsis #'update-synopsis
   :return          (lambda () (return-from main)))

  (unless (length= 1 (remainder))
    (error "Specify exactly one log file."))

  (with-print-limits (*standard-output*)
    (with-logged-warnings
      (let+ ((error-policy (maybe-relay-to-thread
                            (process-error-handling-options)))
             ((input) (remainder))
             (sizes?  (getopt :long-name "compute-sizes"))
             (format? (getopt :long-name "print-format")))
        (with-error-policy (error-policy)
          (with-bag (bag input :direction :input)
            (format t "File ~S~&~2T~<~@;~@{~@(~8A~): ~
                       ~:[N/A~;~:*~,,',:D~]~^~&~}~:>~&~2T~@<~@;~:{Channel ~
                       ~S~&~4T~@<~@;~{~@(~8A~): ~
                       ~:[N/A~;~:*~,,',:D~]~^~&~}~:>~&~}~:>~&"
                    input
                    (let+ (((&accessors-r/o (start rsbag:start)
                                            (end   rsbag:end)) bag)
                           (duration (when (and start end)
                                       (local-time:timestamp-difference
                                        end start))))
                      `(:events ,(reduce #'+ (bag-channels bag) :key #'length)
                        ,@(when sizes?
                            `(:size ,(reduce #'+ (bag-channels bag)
                                             :key #'channel-size)))
                        :start    ,(rsbag:start bag)
                        :end      ,(rsbag:end   bag)
                        :duration ,duration))
                    (iter (for channel each (bag-channels bag))
                          (let+ (((&accessors-r/o (length length)
                                                  (start  rsbag:start)
                                                  (end    rsbag:end)) channel)
                                 (duration (when (and start end)
                                             (local-time:timestamp-difference
                                              end start))))
                            (collect (list (channel-name channel)
                                           `(:type     ,(meta-data channel :type)
                                             :format   ,(when-let ((format (meta-data channel :format)))
                                                          (case format?
                                                            (:short
                                                             (apply #'concatenate 'string
                                                                    (subseq format 0 (min 100 (length format)))
                                                                    (when (> (length format) 100)
                                                                      (list "…"))))
                                                            (:full
                                                             format)))
                                             :events   ,length
                                             ,@(when sizes?
                                                 `(:size ,(channel-size channel)))
                                             :start    ,start
                                             :end      ,end
                                             :duration ,duration
                                             :rate     ,(when (and duration (plusp duration))
                                                          (/ length duration))))))))))))))

;; Local Variables:
;; coding: utf-8
;; End:
