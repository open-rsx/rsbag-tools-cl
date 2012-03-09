;;; help.lisp --- Automatic generation of help strings.
;;
;; Copyright (C) 2012 Jan Moringen
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

(cl:in-package :rsbag.common)

(defun make-replay-strategy-help-string (&key
					 (show :default))
  "Return a help string that explains how to specify replay strategies
and lists the available replay strategies."
  (with-output-to-string (stream)
    (format stream "Replay events form the specified input file ~
according to SPEC which has to be of the form

  KIND KEY1 VALUE1 KEY2 VALUE2 ...

where keys and values depend on KIND and are optional in most ~
cases. Examples (note that the single quotes have to be included only ~
when used within a shell):

  --replay-strategy recorded-timing
  -r as-fast-as-possible
  --replay-strategy 'fixed-rate :rate 10'
  -r 'remote-controlled :uri \"spread://localhost:4803/myplayback/control\"'

")
    (with-abbreviation (stream :strategies show)
      (format stream "Currently, the following strategies are supported:

" )
      (print-classes-help-string
       (rsbag.rsb:replay-strategy-classes) stream
       :initarg-blacklist '(:start-index :end-index
			    :start-time  :end-time
			    :error-policy
			    :stream :previous-command)))))
