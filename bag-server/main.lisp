;;;; main.lisp --- Main function of the bag-server program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2014 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.server)

(defun update-synopsis (&key (show :default))
  "Create and return a commandline option tree."
  (make-synopsis
   :postfix "INPUT-FILE-OR-GLOB-PATTERN-1 [INPUT-FILE-OR-GLOB-PATTERN-2 ...]"
   :item    (make-text :contents (make-help-string :show show))
   :item    (make-common-options :show show)
   :item    (make-error-handling-options :show show)
   :item    (defgroup (:header "Server Options")
                (stropt  :short-name     "a"
                       :long-name      "address"
                       :default-value  "localhost"
                       :argument-name  "BIND-ADDRESS"
                       :description
                       "Address to which to bind to.")
              (lispobj :short-name     "p"
                       :long-name      "port"
                       :typespec       '(integer 0 65535)
                       :default-value  4444
                       :argument-name  "PORT"
                       :description
                       "Port on which to listen.")
              (path    :long-name      "access-log-file"
                       :type           :file
                       :description
                       "Name of a file to which accesses should be logged."))
   ;; Append RSB options.
   :item    (rsb.common::make-options
             :show? (or (eq show t)
                        (and (listp show) (member :rsb show))))
   ;; Append IDL options.
   :item    (make-idl-options)
   ;; Append examples.
   :item    (defgroup (:header "Examples")
              (make-text :contents (make-examples-string)))))

;; TODO(jmoringe, 2012-01-11): duplicated in bag-merge
(defun collect-input-files (args)
  "Collect and return a list of input files according to ARGS.
   ARGS can be
   + A designator of a wild pathname matching one or more files
   + A list of pathname designators of existing files"
  ;; Neither glob expression nor filenames.
  (when (emptyp args)
    (error "~@<No glob expression or one or more input log files ~
            specified.~@:>"))

  (let+ ((parsed (map 'list #'parse-namestring args))
         ((&flet existing-file? (pathname)
            (when-let ((probed (probe-file pathname)))
              (and (pathname-name probed) (pathname-type probed)))))
         ((&flet maybe-resolve (pathname)
            (cond
              ((wild-pathname-p pathname)
               (or (directory pathname)
                   (cerror "Continue without these inputs"
                           "~@<The specified input glob expression ~S did not ~
                            match any files.~@:>"
                           pathname)))
              ((not (existing-file? pathname))
               (cerror "Continue without this input"
                       "~@<The following specified input file ~
                        does not exist: ~S.~@:>"
                       pathname))
              (t
               (list pathname))))))
    (or (mappend #'maybe-resolve parsed)
        (error "~@<No (existing) input files found for ~{~S~^, ~
                ~}.~@:>"
               parsed))))

(defvar *trials* nil
  "TODO(jmoringe): document")

(defun common-prefix (directories)
  "TODO(jmoringe): document"
  (let* ((components (first directories))
         (max-length (length components)))
    (iter (for length :downfrom (1- max-length))
          (when (every (lambda (pathname)
                         (starts-with-subseq
                          (subseq components 0 length) pathname
                          :test #'equal))
                       directories)
            (return length)))))
;; (iter (for other in (rest directories))
;;       (minimizing (mismatch first other)))

(defun strip-common-prefix (directories)
  "TODO(jmoringe): document"
  (map 'list (rcurry #'subseq (common-prefix directories))
       directories))

(defun make-normalized-names (pathnames)
  "TODO(jmoringe): document"
  (let+ (((&flet pathname-components (pathname)
            (append (pathname-directory pathname)
                    (list (pathname-name pathname)
                          (pathname-type pathname)))))
         ((&flet components->string (components)
            (format nil "~{~A~^-~}" components))))
    (map 'list #'components->string
         (strip-common-prefix (map 'list #'pathname-components pathnames)))))

(defun mount-bag (base-uri pathname name)
  (let* ((name/slash (format nil "/~A/" name))
         (bag        (open-bag pathname
                               :direction :input
                               :bag-class 'synchronized-bag
                               :transform `(&from-source
                                            :converter ,(rsb:default-converter 'nibbles:octet-vector))))
         (uri        (puri:merge-uris
                      (make-instance 'puri:uri
                                     :path name/slash)
                      base-uri)))
    (log:info "~@<Serving bag ~A at ~A~@:>" bag uri)
    (push name *trials*)

    (push (hunchentoot:create-prefix-dispatcher
           name/slash
           (rsb.web.rest::make-handler-function
            (rsbag.web.rest::make-bag-handler1 bag)
            :prefix name/slash))
          hunchentoot:*dispatch-table*)

    uri))

(defun main ()
  "Entry point function of the bag-server program."
  (update-synopsis)
  (setf rsb:*default-configuration* (rsb:options-from-default-sources))
  (process-commandline-options
   :version         (cl-rsbag-tools-server-system:version/list :commit? t)
   :more-versions   (list :rsbag         (cl-rsbag-system:version/list :commit? t)
                          :rsbag-tidelog (cl-rsbag-system:version/list :commit? t))
   :update-synopsis #'update-synopsis
   :return          (lambda () (return-from main)))

  ;; Configure webserver.
  (when (getopt :long-name "debug")
    (setf hunchentoot:*show-lisp-errors-p* t))

  (rsb.formatting:with-print-limits (*standard-output*)
    (with-logged-warnings
      ;; Load IDLs as specified on the commandline.
      (process-idl-options)

      (let* ((error-policy (maybe-relay-to-thread
                            (process-error-handling-options)))
             (address      (getopt :long-name "address"))
             (port         (getopt :long-name "port"))
             (acceptor     (apply #'make-instance 'hunchentoot:easy-acceptor
                                  :address address
                                  :port    port
                                  (append
                                   #+later (when-let ((pathname
                                                       (getopt :long-name "message-log-file")))
                                             (list :message-log-destination pathname))
                                   (when-let ((pathname
                                               (getopt :long-name "access-log-file")))
                                     (list :access-log-destination pathname)))))
             (base-url     (make-instance 'puri:uri
                                          :scheme :http
                                          :host    address
                                          :port    port))
             (input-files  (collect-input-files (remainder)))
             (names        (make-normalized-names input-files)))

        (with-error-policy (error-policy)
          (with-interactive-interrupt-exit ()
            (log:info "~@<Preparing to serve files at ~A~@:>" base-url)
            (let+ ((max-length (reduce #'max input-files
                                       :key (compose #'length #'namestring)))
                   (i 0)
                   ((&flet progress (file url)
                      (format *info-output* "~C~4:D ~VA -> ~A"
                              #\Return (incf i) max-length file url)
                      (force-output *info-output*))))
              (iter (for file in input-files)
                    (for name in names)
                    (restart-case
                        (let ((url (mount-bag base-url file name)))
                          (funcall #'progress file url))
                      (continue ()
                        :report (lambda (stream)
                                  (format stream "~@<Continue with the next input.~@:>"))))))
            (terpri *info-output*)

            (push (hunchentoot:create-prefix-dispatcher
                   "/info"
                   (lambda ()
                     (who:with-html-output-to-string (stream)
                       (:html
                        (:body
                         (:ul
                          (dolist (name *trials*)
                            (who:htm
                             (:li (:a :href (format nil "~A/info" name) (who:esc name)))))))))))
                  hunchentoot:*dispatch-table*)

            (log:info "~@<Starting server ~A~@:>" base-url)
            (hunchentoot:start acceptor)

            (unwind-protect
                 (iter (sleep 1))
              (hunchentoot:stop acceptor))))))))
