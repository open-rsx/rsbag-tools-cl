(load-system :asdf-system-connections)

(setf iterate::*always-declare-variables* t)

(load-system :cl-ppcre)       ;; for regex filter

(load-system :cl-protobuf)
(load-system :network.spread) ;; for spread transport
(load-system :usocket)        ;; for socket transport

(load-system :swank)          ;; for the lulz

(unless (asdf:find-system :cl-rsb-common nil)
  (ignore-errors (load-system :cl-rsb-tools)))

(load-system :cl-rsbag-tools-main)

(load-system :cl-rsbag-tidelog)
(load-system :cl-rsbag-elan)

;;; Image saving/resuming logistics

;; Reset ASDF state
(asdf:clear-source-registry)
(asdf:clear-output-translations)

;; Try to reload Spread library
(network.spread:enable-reload-spread-library :if-fails #'warn)

;; Restart rsbag threadpool
(rsbag:enable-restart-threadpool)

(com.dvlsoft.clon:dump "bag" rsbag.tools.main:main
		       :compression :best)
