(load-system :swank) ;; for the lulz

(unless (asdf:find-system :cl-rsb-common nil)
  (load-system-from-artifact :cl-rsb-tools))
(load-system :cl-rsbag-tools-info)

(load-system :cl-rsbag-tidelog)

(asdf:clear-source-registry)
(asdf:clear-output-translations)

(com.dvlsoft.clon:dump "bag-info" rsbag.tools.info:main)
