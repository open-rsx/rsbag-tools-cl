;;; main.lisp --- Dispatch function of the main bag program.
;;
;; Copyright (C) 2011 Jan Moringen
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

(in-package :rsbag.tools.main)

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
  (let* ((pathname (pathname (first (com.dvlsoft.clon::cmdline))))
	 (name     (concatenate 'string
				(pathname-name pathname)
				"."
				(pathname-type pathname)))
	 (entry    (cdr (assoc name *filename->entry-point*
			       :test #'(lambda (name entry)
					 (search entry name))))))
    (if entry
	(funcall entry)
	(format *error-output* "~@<Invoke as ~{~A~^ or ~}.~_~_This is ~
usually done by creating symbolic links~_~_~:*~{~2T~A -> bag~_~}~_The ~
following command can be used to achieve this:~:*~2&~2T~@<~@;~{ln -s ~
bag ~A~^ && ~}~:>~@:>~%"
		(map 'list #'car *filename->entry-point*)))))
