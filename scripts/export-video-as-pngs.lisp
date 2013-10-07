;;;; export-video-as-pngs.lisp --- Export rst.vision.Image payloads as PNG files.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

#.(unless (when-let* ((package (find-package "RST.VISION"))
                      (symbol  (find-symbol "IMAGE-WIDTH" package)))
            (ignore-errors (fdefinition `(setf ,symbol))))
    (error "~@<Class RST.VISION:IMAGE is not loaded properly. Did you ~
load the Image.proto file using the --load-idl option?~@:>"))

(let* (;; Directory into which output images should be written. Note
       ;; trailing `/'.
       (output-directory "./")
       ;; Width and height can be specified as follows:
       ;; + t              -> retain the width/height of the input
       ;;                     image
       ;; + '(:% PERCENT)  -> scale to PERCENT percent of the
       ;;                     width/height of the input image
       ;; + '(:px PIXELS)  -> scale to PIXELS width/height
       ;;
       ;; The latter two forms may not support arbitrary output sizes
       ;; and silently use to the nearest possible sizes when a
       ;; requested size is not supported.
       (style            (make-instance (find-style-class :image/png)
                                        :width  t
                                        :height t))
       ;; Create an output filename within the specified output
       ;; directory based on the timestamp of the current event.
       (filename         (merge-pathnames
                          (format nil "~A.~A"
                                  (floor create-unix/nsec 1000) "png")
                          output-directory)))

  (format t "~CProcessing frame ~A -> ~A"
          #\Return create-unix/nsec filename)
  (force-output *standard-output*)

  ;; Make sure the output directory actually exists, creating it if
  ;; necessary.
  (ensure-directories-exist output-directory)

  ;; Write the payload of the current event into the output file,
  ;; formatting the data as a PNG image.
  (with-output-to-file (stream filename
                               :if-exists         :overwrite
                               :if-does-not-exist :create
                               :element-type      '(unsigned-byte 8))
    (format-payload data style stream)))
