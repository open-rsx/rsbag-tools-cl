(load-system :asdf-system-connections)

(load-system :cl-ppcre)    ;; for regex filter

(load-system :cl-protobuf)
(load-system :cl-spread)   ;; for spread transport
(load-system :usocket)     ;; for socket transport

(load-system :swank)       ;; for the lulz

(unless (asdf:find-system :cl-rsb-common nil)
  (ignore-errors (load-system :cl-rsb-tools)))

(load-system :cl-rsbag-tools-main)

(load-system :cl-rsbag-tidelog)
(load-system :cl-rsbag-elan)

(asdf:clear-source-registry)
(asdf:clear-output-translations)
(cffi:close-foreign-library 'spread::libspread)

(defun reload-spread-and-main ()
  (handler-case
      (cffi:use-foreign-library spread::libspread)
    (error (condition)
      (warn "~@<Failed to load Spread library: ~A. Did you set ~
LD_LIBRARY_PATH? ~_Spread transport will now be disabled.~@:>"
	    condition)))
  (rsbag.tools.main:main))

(com.dvlsoft.clon:dump "bag" reload-spread-and-main
		       #+sb-core-compression :compression
		       #+sb-core-compression 9)
