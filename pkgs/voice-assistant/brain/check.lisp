;;; Nix build: load with warnings as errors and test, in a process of its
;;; own, so the saved image carries no tests.

(require :asdf)
(asdf:load-asd (merge-pathnames "voice-assistant-brain.asd" (uiop:getcwd)))
(load (merge-pathnames "strict.lisp" (uiop:getcwd)))
(unless (uiop:symbol-call :va :check)
  (uiop:quit 1))
