;;;; export-audio-as-wav.lisp --- Export rst.audition.SoundChunk as WAV stream.
;;;;
;;;; Copyright (C) 2012, 2013 Jan Moringen
;;;;
;;;; Author: Jan Moringen <jmoringe@techfak.uni-bielefeld.de>

;;;; Note: this script exists mainly for educational purposes. In most
;;;; cases, the :audio-stream/wav formatting style can be used
;;;; instead.

#.(unless (when-let* ((package (find-package "RST.AUDITION"))
                      (symbol  (find-symbol "SOUND-CHUNK-CHANNELS" package)))
            (ignore-errors (fdefinition `(setf ,symbol))))
    (error "~@<Class RST.AUDITION:SOUND-CHUNK is not loaded ~
properly. Did you load the SoundChunk.proto file using the --load-idl ~
option?~@:>"))

(let+ (((&accessors-r/o (channels      rst.audition:sound-chunk-channels)
                        (sample-rate   rst.audition:sound-chunk-rate)
                        (sample-format rst.audition:sound-chunk-sample-type)
                        (samples       rst.audition:sound-chunk-data))
        data)
       (width     (ecase sample-format
                    ((:sample-s8  :sample-u8)  1)
                    ((:sample-s16 :sample-u16) 2)
                    ((:sample-s24 :sample-u24) 3)))
       (byte-rate (* channels sample-rate width)))

  ;; Write WAV header, if we have not done that yet. We record the
  ;; fact that the header has been written by binding the variable
  ;; header-written? to t.
  (unless (boundp 'header-written?)
    (format *error-output* "Writing wave header for ~A (~D ~
                            byte~:P/sample) x ~D channel~:P @ ~:D Hz~%"
            sample-format width channels sample-rate)
    (format *error-output* "Begin timestamp   ~A~%" create)

    ;; The non-constant fields have the following meanings:
    ;; 0 Size
    ;; 1 Format
    ;; 2 Number of Channels
    ;; 3 Sample Rate
    ;; 4 Byte Rate
    ;; 5 Block Alignment
    ;; 6 Bits per Sample
    ;; 7 Sub Chunk Size
    (let ((wave-header-template
           (vector #x52 #x49 #x46 #x46 0    0    0    0    #x57 #x41 #x56 #x45
                   #x66 #x6d #x74 #x20 #x10 #x00 #x00 #x00 1    1    2    2
                   3    3    3    3    4    4    4    4    5    5    6    6
                   #x64 #x61 #x74 #x61 7    7    7    7))
          (fake-size #x7fffffff))  ;; Assumed maximum possible size)
      (macrolet
          ((set-field (start size value)
             (once-only (value)
               `(iter (for i :below ,size)
                      (setf (aref wave-header-template (+ ,start i))
                            (ldb (byte 8 (* i 8)) ,value))))))

        ;; Regarding "Size" and "Sub chunk size": We use
        ;;
        ;;   Size:           fake-size
        ;;   Sub chunk size: (- fake-size 44)
        ;;
        ;; where SIZE is the assumed maximum size, since we cannot know
        ;; the file size in advance. This seems to make most software do
        ;; the right thing.
        (set-field  4 4 fake-size)         ; Size.
        (set-field 20 2 1)                 ; PCM format
        (set-field 22 2 channels)          ; Number of channels
        (set-field 24 4 sample-rate)       ; Sample rate
        (set-field 28 4 byte-rate)         ; Byte rate
        (set-field 32 2 width)             ; Block alignment
        (set-field 34 2 (* width 8))       ; Bits per sample
        (set-field 40 4 (- fake-size 44))) ; Sub chunk size
      (write-sequence wave-header-template stream))
    (set 'header-written? t))

  (format *error-output* "~CProcessing buffer ~A" #\Return create)
  (force-output *error-output*)

  (write-sequence samples stream))
