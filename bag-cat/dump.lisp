(load-system :asdf-system-connections)

(load-system :swank) ;; for the lulz

(unless (asdf:find-system :cl-rsb-common nil)
  (ignore-errors (load-system :cl-rsb-tools)))
(load-system :cl-rsbag-tools-cat)

(load-system :cl-rsbag-tidelog)

(asdf:clear-source-registry)
(asdf:clear-output-translations)

(com.dvlsoft.clon:dump "bag-cat" rsbag.tools.cat:main
		       :compression :best)
