;;;; main.lisp --- Dispatch function of the main bag program.
;;;;
;;;; Copyright (C) 2011, 2012, 2013, 2015 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

(cl:in-package #:rsbag.tools.main)

(defvar *name->entry-point*
  '(("record"    . rsbag.tools.record:main)
    ("info"      . rsbag.tools.info:main)
    ("merge"     . rsbag.tools.merge:main)
    ("transform" . rsbag.tools.merge:main)
    ("cat"       . rsbag.tools.cat:main)
    ("play"      . rsbag.tools.play:main))
  "Stores a mapping from program names to entry point functions.")

(defun program-pathname->name (program-pathname)
  (apply #'concatenate 'string (pathname-name program-pathname)
         (when (pathname-type program-pathname)
           (list "." (pathname-type program-pathname)))))

(defun main ()
  "Entry point function of the main bag program."
  (make-synopsis)
  (let+ (((program-name &rest args) (com.dvlsoft.clon::cmdline))
         (program-pathname (pathname program-name)))
    (cond
      ;; If the program name does not correspond to an entry-point,
      ;; try to use the first commandline option as a sub-command.
      ((when-let* ((command (first args))
                   (entry   (assoc command *name->entry-point*
                                   :test #'string=)))
         (funcall (cdr entry) program-pathname (rest args))
         t))

      ;; If the program has been called with the "redump" commandline
      ;; option, dump into a new binary with the specified library
      ;; loading behavior and the specified core compression.
      ((string= "redump" (first args))
       (let+ (((&optional (name (program-pathname->name
                                 program-pathname))
                          &rest local-args)
               (rest args))
              (static?   (when (member "static" local-args :test #'string=)
                           t))
              (compress? (member "compress" local-args :test #'string=)))
         (rsb.tools.commands:command-execute
          (rsb.tools.commands:make-command
           :redump
           :output-file name
           :static?     static?
           :compression (when compress? 9)))))

      ;; Otherwise display information regarding entry points and
      ;; symbolic links and offer to create these automatically if
      ;; necessary.
      (t
       (format *error-output* "~@<Invoke this program as~
                               ~@:_~@:_~
                               ~5@T~A redump [FILENAME (compress|static)*]~
                               ~{~@:_  or ~{~A ~A~}~}~
                               ~@:_~@:_~
                               (not ~2:*~A).~@:>~%"
               program-pathname
               (mapcar (lambda (entry)
                         (list program-pathname (car entry)))
                       *name->entry-point*))))))
