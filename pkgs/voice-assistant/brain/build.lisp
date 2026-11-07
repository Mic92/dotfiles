;;; Nix build: load the brain without its tests and save the executable.

(require :asdf)
(asdf:load-asd (merge-pathnames "voice-assistant-brain.asd" (uiop:getcwd)))
(load (merge-pathnames "strict.lisp" (uiop:getcwd)))
;; Built into the image, so the running brain never conses through them.
(uiop:symbol-call :va :warm-caches)
(sb-ext:save-lisp-and-die (second sb-ext:*posix-argv*)
                          :executable t
                          :toplevel (lambda () (uiop:symbol-call :va :main)))
