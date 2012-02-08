;;; export-pngs.lisp --- Export rst.vision.Image payloads as PNG files.
;;
;; Copyright (C) 2012 Jan Moringen
;;
;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>
;;
;; This Program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This Program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses>.


;; Directory into which output images should be written. Note trailing
;; `/'.
;;
;; Width and height can be specified as follows:
;; + t              -> retain the width/height of the input image
;; + '(:% PERCENT)  -> scale to PERCENT percent of the width/height of
;;                     the input image
;; + '(:px PIXELS)  -> scale to PIXELS width/height
;;
;; The latter two forms may not support arbitrary output sizes and
;; silently use to the nearest possible sizes when a requested size is
;; not supported.
(let ((output-directory "./")
      (style            (make-instance (rsb.formatting:find-style-class :image/png)
				       :width  t
				       :height t)))

  ;; Make sure the output directory actually exists, creating it if
  ;; necessary.
  (ensure-directories-exist output-directory)

  (format *standard-output* "Processing frame ~A~%" create-unix-nsec)

  ;; Create an output filename within the specified output directory
  ;; based on the timestamp of the current event.
  (let ((filename (merge-pathnames
		   (format nil "~A.~A" (floor create-unix-nsec 1000) "png")
		   output-directory)))
    ;; Write the payload of the current event into the output file,
    ;; formatting the data as a PNG image.
    (with-output-to-file (stream filename
				 :if-exists         :overwrite
				 :if-does-not-exist :create
				 :element-type      '(unsigned-byte 8))
      (format-payload (pb:unpack data 'rst.vision:image) style stream))))
