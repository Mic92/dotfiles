;;; The brain's entry point: the protocol socket, the eval socket, systemd.

(in-package #:va)

(defn warm-caches () null
  "Build what is kept until a command changes, as for the saved image. At
midnight every \"today at 7 pm\" is still ahead, so those examples parse."
  (let ((*context* (make-context 0 0)))
    (classifier-examples)
    (lexicon))
  nil)

(defn tell-systemd-ready () null
  "Under systemd with Type=notify, the assistant waits for this before it
connects, instead of starting a private brain."
  (when (and (uiop:getenv "NOTIFY_SOCKET") (null (run "systemd-notify" "--ready")))
    (format *error-output* "va-brain: systemd-notify failed~%"))
  nil)

(defn main () nil
  "Entry point of the brain. With --socket PATH it only speaks the protocol
there, as a private brain for one client, and exits with it, otherwise it is the live brain."
  (setf *random-state* (make-random-state t))
  ;; The image remembers the build's cache dir, which `reload' would write to.
  (uiop:call-image-restore-hook)
  (asdf:clear-output-translations)
  ;; SBCL keeps every page it ever touched. A smaller nursery than the
  ;; default 5% of the heap keeps that near 80 MB.
  (setf (sb-ext:bytes-consed-between-gcs) (* 16 1024 1024))
  (let ((private (second (member "--socket" sb-ext:*posix-argv* :test #'string=))))
    (unless private
      (sb-thread:make-thread (lambda () (serve (merge-pathnames "eval.sock" (runtime-dir))))
                             :name "va-eval server"))
    (cond (private (serve-one (pathname private) #'converse)
                   (sb-ext:exit :code 0))
          (t (let ((server (listening-socket (merge-pathnames "brain.sock" (runtime-dir)))))
               (tell-systemd-ready)
               (serve-socket server "va-protocol" #'converse))))))
