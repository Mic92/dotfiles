;;; `va-dev`: load the brain from the working tree into a core that
;;; already has the libraries, then answer `va-eval` on dev.sock.

;; The core remembers the build's cache dir.
(require :asdf)
(uiop:call-image-restore-hook)
(asdf:clear-output-translations)
(let ((src (uiop:ensure-directory-pathname (uiop:getenv "VA_BRAIN_SRC"))))
  (asdf:load-asd (merge-pathnames "voice-assistant-brain.asd" src)))
(setf *random-state* (make-random-state t))
(let ((*compile-verbose* nil) (*compile-print* nil))
  (asdf:load-system "voice-assistant-brain"))
(let ((warnings (uiop:symbol-call :va :reload))
      (sock (merge-pathnames "dev.sock" (uiop:symbol-call :va :runtime-dir))))
  (dolist (w warnings) (format *error-output* "~a~%" w))
  (format t "va-dev: ~a~%" sock)
  (finish-output)
  (uiop:symbol-call :va :serve sock))
