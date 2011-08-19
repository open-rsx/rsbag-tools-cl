(load-system :cl-ppcre) ;; for regex filter

(load-system :cl-spread) ;; for spread transport

(load-system :swank) ;; for the lulz

(load-system :cl-rsbag-tools-record)

(load-system :cl-rsbag-tidelog)
(load "~/code/cl-rsbag/trunk/src/rsb.lisp")

(asdf:clear-source-registry)
(asdf:clear-output-translations)
(cffi:close-foreign-library 'spread::libspread)

(defun reload-spread-and-main ()
  (cffi:use-foreign-library spread::libspread)
  (rsbag.tools.record:main))

(com.dvlsoft.clon:dump "bag-record" reload-spread-and-main)
