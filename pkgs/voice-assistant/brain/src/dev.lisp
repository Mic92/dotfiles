(in-package #:va)

;;; The eval socket: one form per connection, plain text back.

(declaim (ftype (function () (values pathname &optional)) runtime-dir))
(defun runtime-dir ()
  (merge-pathnames "voice-assistant/"
                   (uiop:ensure-directory-pathname
                    (or (uiop:getenv "XDG_RUNTIME_DIR") "/tmp/"))))

(declaim (ftype (function (condition) (values string &optional)) short-backtrace))
(defun short-backtrace (c)
  (with-output-to-string (s)
    (format s "~a: ~a~%" (type-of c) c)
    (sb-debug:print-backtrace :stream s :count 15)))

(declaim (ftype (function (string) (values string &optional)) eval-string))
(defun eval-string (text)
  "Every form in TEXT, in package VA. Output, then the last values."
  (let ((*package* (find-package '#:va)))
    (with-output-to-string (*standard-output*)
      (let ((*error-output* *standard-output*)
            (*fake-effects* (or *fake-effects* (list :recording))))
        (handler-case
            (sb-ext:with-timeout 10
              (with-input-from-string (in text)
                (let (values)
                  (loop for form = (read in nil in)
                        until (eq form in)
                                        ; mallet:suppress no-eval -- the eval socket
                        do (setf values (multiple-value-list (eval form))))
                  (format t "~&~{~s~^~%~}~%" values))))
          (sb-ext:timeout () (format t "~&timed out after 10 s~%"))
          (error (c) (format t "~&~a" (short-backtrace c))))))))

(declaim (ftype (function (stream) (values string &optional)) read-all))
(defun read-all (stream)
  (with-output-to-string (out)
    (loop for line = (read-line stream nil) while line do (write-line line out))))

(declaim (ftype (function (pathname) nil) serve))
(defun serve (path)
  "Answer eval requests on the Unix socket PATH, each in its own thread."
  (ensure-directories-exist path)
  (when (probe-file path) (delete-file path))
  (let ((server (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-bind server (namestring path))
    (sb-bsd-sockets:socket-listen server 8)
    (loop
     (let ((client (sb-bsd-sockets:socket-accept server)))
       (sb-thread:make-thread
        (lambda ()
          (unwind-protect
               (let ((s (sb-bsd-sockets:socket-make-stream
                         client :input t :output t :external-format :utf-8)))
                 (write-string (eval-string (read-all s)) s)
                 (finish-output s))
            (sb-bsd-sockets:socket-close client)))
        :name "va-eval")))))

(declaim (ftype (function () (values list &optional)) reload))
(defun reload ()
  "Recompile changed files and the ones after them (the system is serial,
so Coalton types are rechecked). Returns the warnings as strings."
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

(declaim (ftype (function () (values boolean &optional)) check))
(defun check ()
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
        (format t "FAIL ~a~%" (uiop:symbol-call :parachute :format-result f :oneline)))
      (format t "~d warnings, ~d passed, ~d failed~%"
              (length warnings) (length passed) (length failed))
      (and (null warnings) (null failed)))))

(declaim (ftype (function () nil) main))
(defun main ()
  "Entry point of the live brain."
  (setf *random-state* (make-random-state t))
  (serve (merge-pathnames "brain.sock" (runtime-dir))))
