;;; Nix build: load the brain without its tests and save the executable.

(require :asdf)
(asdf:load-asd (merge-pathnames "voice-assistant-brain.asd" (uiop:getcwd)))
(load (merge-pathnames "strict.lisp" (uiop:getcwd)))
;; Built into the image, so the running brain never conses through them.
(uiop:symbol-call :va :warm-caches)
;; Libraries never change under a saved brain, so `reload' does not
;; recompile them into the user's cache.
(dolist (s (asdf:already-loaded-systems))
  (unless (search "voice-assistant" s) (asdf:register-immutable-system s)))
(asdf:clear-configuration)
(sb-ext:save-lisp-and-die (second sb-ext:*posix-argv*)
                          :executable t
                          :toplevel (lambda () (uiop:symbol-call :va :main)))
