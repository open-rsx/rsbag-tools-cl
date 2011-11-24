(load-system :cl-ppcre) ;; for regex filter

(load-system :swank)    ;; for the lulz

(unless (asdf:find-system :cl-rsb-common nil)
  (ignore-errors (load-system :cl-rsb-tools)))
(load-system :cl-rsbag-tools-merge)

(load-system :cl-rsbag-tidelog)

(asdf:clear-source-registry)
(asdf:clear-output-translations)

(com.dvlsoft.clon:dump "bag-merge" rsbag.tools.merge:main)
