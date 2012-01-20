(load-system :asdf-system-connections)

(load-system :cl-ppcre)    ;; for regex filter

(load-system :cl-protobuf) ;; for spread and socket transport
(load-system :cl-spread)   ;; for spread transport

(load-system :usocket)     ;; for socket transport

(load-system :swank)       ;; for the lulz

(unless (asdf:find-system :cl-rsb-common nil)
  (ignore-errors (load-system :cl-rsb-tools)))
(load-system :cl-rsbag-tools-record)

(load-system :cl-rsbag-tidelog)

(asdf:clear-source-registry)
(asdf:clear-output-translations)
(cffi:close-foreign-library 'spread::libspread)

(defun reload-spread-and-main ()
  (cffi:use-foreign-library spread::libspread)
  (rsbag.tools.record:main))

(com.dvlsoft.clon:dump "bag-record" reload-spread-and-main)
