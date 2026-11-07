(in-package #:va)

;;; The eval socket: one form per connection, plain text back.

(defn runtime-dir () pathname
  (merge-pathnames "voice-assistant/"
                   (uiop:ensure-directory-pathname
                    (or (uiop:getenv "XDG_RUNTIME_DIR") "/tmp/"))))

(defn short-backtrace ((c condition)) string
  (with-output-to-string (s)
    (format s "~a: ~a~%" (type-of c) c)
    (sb-debug:print-backtrace :stream s :count 15)))

(defn untimed-p ((form t)) boolean
  "Whether FORM may run past the eval timeout: a reload compiles."
  (and (consp form) (eq (first form) 'reload)))

(defn eval-string ((text string)) string
  "Every form in TEXT, in package VA, each cut off after 10 s unless
`untimed-p'. Output, then the last values."
  (let ((*package* (find-package '#:va)))
    (with-output-to-string (*standard-output*)
      (let ((*error-output* *standard-output*)
            (*fake-effects* (or *fake-effects* (list :recording))))
        (handler-case
            (with-input-from-string (in text)
              (let (values)
                (loop for form = (read in nil in)
                      until (eq form in)
                      do (setf values
                               (if (untimed-p form)
                                        ; mallet:suppress no-eval -- the eval socket
                                   (multiple-value-list (eval form))
                                   (sb-ext:with-timeout 10
                                        ; mallet:suppress no-eval -- the eval socket
                                     (multiple-value-list (eval form))))))
                (format t "~&~{~s~^~%~}~%" values)))
          (sb-ext:timeout () (format t "~&timed out after 10 s~%"))
          (error (c) (format t "~&~a" (short-backtrace c))))))))

(defn read-all ((stream stream)) string
  (with-output-to-string (out)
    (loop for line = (read-line stream nil) while line do (write-line line out))))

(defn serve ((path pathname)) nil
  "Answer eval requests on the Unix socket PATH, each in its own thread."
  (accept-on path "va-eval"
             (lambda (s)
               (write-string (eval-string (read-all s)) s)
               (finish-output s))))

(defn reload (&optional (source (or null string pathname) nil)) list
  "Recompile changed files and the ones after them (the system is serial,
so Coalton types are rechecked). Returns the warnings as strings. SOURCE, the brain's directory in a
checkout, points a brain built by nix at it first."
  (when source
    (asdf:clear-system "voice-assistant-brain")
    (asdf:load-asd (merge-pathnames "voice-assistant-brain.asd"
                                    (uiop:ensure-directory-pathname source))))
  (let (warnings)
    (handler-bind ((sb-kernel:redefinition-warning #'muffle-warning)
                   (warning
                    (lambda (w)
                      (push (format nil "~@[~a: ~]~a"
                                    (and *compile-file-truename*
                                         (enough-namestring *compile-file-truename*
                                                            (asdf:system-source-directory
                                                             "voice-assistant-brain")))
                                    w)
                            warnings)
                      (muffle-warning w))))
      (let ((*compile-verbose* nil) (*compile-print* nil))
        (asdf:load-system "voice-assistant-brain")))
    (nreverse warnings)))

(defn check () boolean
  "Reload, then run the tests. Prints failures and one summary line."
  (let ((warnings (reload)))
    (dolist (w warnings) (format t "~a~%" w))
    (let ((*compile-verbose* nil) (*compile-print* nil))
      (asdf:load-system "voice-assistant-brain/tests"))
    (let* ((report (uiop:symbol-call :parachute :test :va-tests
                                     :report (uiop:find-symbol* :quiet :parachute)))
           (failed (uiop:symbol-call :parachute :results-with-status :failed report))
           (passed (uiop:symbol-call :parachute :results-with-status :passed report)))
      (dolist (f failed)
        (format t "FAIL ~a~@[  ~a~]~%" (uiop:symbol-call :parachute :format-result f :oneline)
                (uiop:symbol-call :parachute :description f)))
      (format t "~d warnings, ~d passed, ~d failed~%"
              (length warnings) (length passed) (length failed))
      (and (null warnings) (null failed)))))

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
  (when (uiop:getenv "NOTIFY_SOCKET")
    (handler-case (sb-ext:run-program "systemd-notify" '("--ready") :search t)
      (error (e) (format *error-output* "va-brain: systemd-notify: ~a~%" e))))
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
