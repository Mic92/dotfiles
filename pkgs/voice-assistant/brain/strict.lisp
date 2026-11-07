;;; Load the brain with warnings other than style warnings as errors.

(setf sb-ext:*derive-function-types* t)
(handler-bind ((warning (lambda (w)
                          (unless (typep w 'style-warning)
                            (format *error-output* "~&error: ~a~%" w)
                            (uiop:quit 1)))))
  (asdf:load-system "voice-assistant-brain"))
