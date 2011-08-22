(load-system :cl-ppcre) ;; for regex filter

(load-system :cl-spread) ;; for spread transport

(load-system :swank) ;; for the lulz

(unless (asdf:find-system :cl-rsb-common nil)
  (load-system-from-artifact :cl-rsb-tools))
(load-system :cl-rsbag-tools-play)

(load-system :cl-rsbag-tidelog)

(asdf:clear-source-registry)
(asdf:clear-output-translations)
(cffi:close-foreign-library 'spread::libspread)

(defun reload-spread-and-main ()
  (cffi:use-foreign-library spread::libspread)
  (rsbag.tools.play:main))

(com.dvlsoft.clon:dump "bag-play" reload-spread-and-main)
