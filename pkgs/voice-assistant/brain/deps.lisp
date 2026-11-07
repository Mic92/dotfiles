;;; Nix build: a core with the libraries loaded, for `va-dev`.

(require :asdf)
(dolist (s '("coalton" "alexandria" "trivia" "com.inuoe.jzon" "parachute" "cl-ppcre"
             "sb-bsd-sockets" "sb-introspect"))
  (asdf:load-system s))
(asdf:clear-configuration)
(sb-ext:save-lisp-and-die (second sb-ext:*posix-argv*))
