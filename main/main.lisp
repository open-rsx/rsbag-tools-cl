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

(defun main/program-name (program-pathname args)
  (when-let* ((name  (pathname-name program-pathname))
              (entry (assoc name *name->entry-point*
                            :test (lambda (name entry)
                                    (search entry name)))))
    (let ((main-pathname #+unix (truename (format nil "/proc/~D/exe" (sb-posix:getpid)))
                         #-unix "bag"))
      (warn "~@<Selecting the command to execute via the program ~
             name (i.e. running ~A~@[ ~{~A~^ ~}~]) is deprecated. Use~
             ~@:_~@:_~
             ~2@T~@<~A ~A~@[ ~{~A ~^~}~]~:>~
             ~@:_~@:_~
             instead.~@:>"
            program-pathname args main-pathname (car entry) args)
      (funcall (cdr entry) (program-pathname->name main-pathname) args))
    t))

(defun main/command (program-pathname command args)
  (when-let ((entry (assoc command *name->entry-point* :test #'string=)))
    (funcall (cdr entry) program-pathname args)
    t))

(defun main ()
  "Entry point function of the main bag program."
  (make-synopsis)
  (let+ (((program-name &rest args) (com.dvlsoft.clon::cmdline))
         (program-pathname (pathname program-name)))
    (cond
      ;; If we can find an entry point based on our program name, use
      ;; it.
      ((main/program-name program-pathname args))

      ;; If the program name does not correspond to an entry-point,
      ;; try to use the first commandline option as a sub-command.
      ((when-let ((command (first args)))
         (main/command program-pathname command (rest args))))

      ;; If the program has been called with the "create-links"
      ;; commandline option, create symbolic links for entry points as
      ;; necessary and exit.
      ((string= "create-links" (first args))
       (warn "~@<Selecting the command to execute via the program name ~
              is deprecated.~@:>")
       (%maybe-create-links program-pathname (second args) (third args)))

      ;; If the program has been called with the "redump" commandline
      ;; option, dump into a new binary with the specified library
      ;; loading behavior and the specified core compression.
      ((string= "redump" (first args))
       (let+ (((&optional (name (program-pathname->name
                                 program-pathname))
                          &rest local-args)
               (rest args))
              (static?   (member "static" local-args :test #'string=))
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
                               ~5@T~A create-links [PREFIX [SUFFIX]]       (Deprecated)~
                               ~@:_  or ~:*~A redump [FILENAME (compress|static)*]~
                               ~{~@:_  or ~{~A ~A~}~}~
                               ~@:_~@:_~
                               (not ~2:*~A).~@:>~%"
               program-pathname
               (mapcar (lambda (entry)
                         (list program-pathname (car entry)))
                       *name->entry-point*))))))

;;; Utility functions

(defun %maybe-create-link (target name &optional prefix suffix)
  "If NAME does not designate a filesystem object, create a symbolic
   link to TARGET named NAME. Note that existing filesystem objects
   named NAME can prevent the creation of the symbolic link."
  (let ((name (format nil "~@[~A~]~A~@[~A~]" prefix name suffix)))
    (unless (probe-file name)
      #-(and sbcl (not win32)) (error "~@<Don't know how to create ~
                                        symbolic links on this ~
                                        implementation-platform ~
                                        combination.~@:>")
      (format t "~@<Creating symbolic link ~A -> ~A~@:>~%"
              name target)
      #+(and sbcl (not win32)) (sb-posix:symlink target name))))

(defun %maybe-create-links (target &optional prefix suffix)
  "Create symbolic links to TARGET for each entry in
   `*name->entry-point*', if necessary."
  (let ((names (mapcar #'car *name->entry-point*)))
    (mapc (rcurry #'%maybe-create-link prefix suffix)
          (circular-list target) names)))
