;;; Nix build: load with warnings as errors, test, save the executable.

(require :asdf)
(asdf:load-asd (merge-pathnames "voice-assistant-brain.asd" (uiop:getcwd)))
(setf sb-ext:*derive-function-types* t)
(handler-bind ((warning (lambda (w)
                          (unless (typep w 'style-warning)
                            (format *error-output* "~&error: ~a~%" w)
                            (uiop:quit 1)))))
  (asdf:load-system "voice-assistant-brain"))
(unless (uiop:symbol-call :va :check)
  (uiop:quit 1))
(sb-ext:save-lisp-and-die (second sb-ext:*posix-argv*)
                          :executable t
                          :toplevel (lambda () (uiop:symbol-call :va :main)))
