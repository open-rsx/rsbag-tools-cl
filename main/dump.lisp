(load-system :iterate)
(setf iterate::*always-declare-variables* t)

(load-system :cl-ppcre)       ; for regex filter

(load-system :cl-protobuf)
#-win32 (load-system :network.spread) ; for spread transport
(load-system :usocket)        ; for socket transport

(load-system :swank)          ; for the lulz

(unless (asdf:find-system :cl-rsb-common nil)
  (ignore-errors (load-system :cl-rsb-tools)))

(load-system :cl-rsbag-tools-main)

;;; Image saving/resuming logistics

;; Reset ASDF state
(asdf:clear-source-registry)
(asdf:clear-output-translations)

;; Reseed RSB id generator
(rsb:enable-id-random-state-reseed)

;; As a default, try reloading foreign libraries on startup. If
;; necessary, users can change this by "redumping".
(rsbag.tools.main:make-dynamic)

;; Restart rsbag threadpool
(rsbag:enable-restart-threadpool)

(com.dvlsoft.clon:dump #-win32 "bag" #+win32 "bag.exe"
                       rsbag.tools.main:main)
