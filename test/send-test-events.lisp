(defun send-test-event-and-exit ()
  (sleep 1)
  (rsb:with-participant (i :informer "inprocess:")
    (let ((buffer (nibbles:make-octet-vector 4)))
      (setf (nibbles:ub32ref/le buffer 0) 1)
      (rsb:send i buffer :rsb.transport.wire-schema "UINT32")))
  (sleep 1)
  (sb-ext:exit))

(let ((timer (sb-ext:make-timer #'send-test-event-and-exit)))
  (sb-ext:schedule-timer timer 2))
