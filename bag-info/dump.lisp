(load-system :swank) ;; for the lulz

(load-system :cl-rsbag-tools-info)

(load-system :cl-rsbag-tidelog)

(asdf:clear-source-registry)
(asdf:clear-output-translations)

(com.dvlsoft.clon:dump "bag-info" rsbag.tools.info:main)
