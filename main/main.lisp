;;; main.lisp --- Dispatch function of the main bag program.
;;
;; Copyright (C) 2011, 2012, 2013 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.

(cl:in-package :rsbag.tools.main)

(defvar *filename->entry-point*
  '(("bag-record" . rsbag.tools.record:main)
    ("bag-info"   . rsbag.tools.info:main)
    ("bag-merge"  . rsbag.tools.merge:main)
    ("bag-cat"    . rsbag.tools.cat:main)
    ("bag-play"   . rsbag.tools.play:main))
  "Stores a mapping from program names to entry point functions.")

(defun main ()
  "Entry point function of the main bag program."
  (make-synopsis)
  (let* ((args     (com.dvlsoft.clon::cmdline))
	 (pathname (pathname (first args)))
	 (name     (apply #'concatenate
			  'string
			  (pathname-name pathname)
			  (when (pathname-type pathname)
			    (list "." (pathname-type pathname)))))
	 (entry    (cdr (assoc name *filename->entry-point*
			       :test #'(lambda (name entry)
					 (search entry name))))))
    (cond
      ;; If we found an entry point, use it.
      (entry
       (funcall entry))

      ;; If the program has been called with the "create-links"
      ;; commandline option, create symbolic links for entry points as
      ;; necessary and exit.
      ((string= "create-links" (second args))
       (%maybe-create-links name))

      ;; If the program has been called with the "redump" commandline
      ;; option, dump into a new binary with the specified library
      ;; loading behavior and the specified core compression.
      ((string= "redump" (second args))
       (destructuring-bind (&optional (name name) &rest local-args)
	   (nthcdr 2 args)

	 ;; Change behavior for foreign libraries. Either hard-wire
	 ;; their names into the dumped image ("static") or search for
	 ;; them on image restart.
	 (if (member "static" local-args :test #'string=)
	     (make-static)
	     (make-dynamic))

	 ;; Create new binary.
	 (eval
	  `(com.dvlsoft.clon:dump
	    ,name main
	    :compression ,(when (member "compress" local-args :test #'string=)
			    :best)))))

      ;; Otherwise display information regarding entry points and
      ;; symbolic links and offer to create these automatically if
      ;; necessary.
      (t
       (format *error-output* "~@<Invoke this program as~_~_~
~5@T~A create-links~
~_  or ~:*~A redump [FILENAME (compress|static)*]~
~{~_  or ~A~}~_~_(not ~2:*~S). The latter invocations are usually ~
done by creating symbolic links~_~_~
~{~2@T~A -> bag~_~}~@:>~%"
	       name
	       (mapcar #'car *filename->entry-point*))
       (unless (every (compose #'probe-file #'car) *filename->entry-point*)
	 (format *query-io* "Create missing links now [yes/no]? ")
	 (finish-output *query-io*)
	 (when (member (read-line *query-io*) '("y" "yes")
		       :test #'equal)
	   (%maybe-create-links name)))))))


;;; Library loading behavior
;;

(defun make-static ()
  "Hard-wire locations of foreign libraries."
  ;; Do not reload Spread library.
  #-win32 (unless (network.spread-system:spread-library-pathname)
	    (error "~@<Spread library pathname not provided (use ~
SPREAD_LIBRARY environment variable).~@:>"))

  #-win32 (network.spread:use-spread-library
	   :pathname (network.spread-system:spread-library-pathname))
  #-win32 (network.spread:disable-reload-spread-library))

(defun make-dynamic ()
  "Enable dynamic search for and loading of foreign libraries."
  ;; Try to reload Spread library.
  #-win32 (ignore-errors
	   (network.spread:use-spread-library :pathname nil))
  #-win32 (network.spread:enable-reload-spread-library :if-fails #'warn))


;;; Utility functions
;;

(defun %maybe-create-link (target name)
  "If NAME does not designate a filesystem object, create a symbolic
link to TARGET named NAME. Note that existing filesystem objects named
NAME can prevent the creation of the symbolic link."
  (unless (probe-file name)
    #-(and sbcl (not win32)) (error "~@<Don't know how to create ~
symbolic links on this implementation-platform combination.~@:>")
    (format t "~@<Creating symbolic link ~A -> ~A~@:>~%"
	    name target)
    #+(and sbcl (not win32)) (sb-posix:symlink target name)))

(defun %maybe-create-links (target)
  "Create symbolic links to TARGET for each entry in
`*filename->entry-point*', if necessary."
  (let ((names (mapcar #'car *filename->entry-point*)))
    (map nil #'%maybe-create-link (circular-list target) names)))
