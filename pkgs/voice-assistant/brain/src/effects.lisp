(in-package #:va)

;;; Every side effect goes through `run`, so dev brains and tests only
;;; record what would have happened.

(defvar *fake-effects* nil
  "When a list, `run` pushes (program . args) onto it instead of running.")

(defmacro fake-effects (&body body)
  "Run BODY recording effects. Returns them, oldest first."
  `(let ((*fake-effects* (list :recording)))
     ,@body
     (reverse (butlast *fake-effects*))))

(defmacro fake-effects-value (&body body)
  "BODY's value, its effects only recorded."
  `(let ((*fake-effects* (list :recording))) ,@body))

(declaim (ftype (function () (values list &optional)) effects))
(defun effects () *fake-effects*)

(declaim (ftype (function (string &rest string) (values (or null string) &optional)) run))
(defun run (program &rest args)
  "Run PROGRAM with ARGS, without a shell. Returns its stdout, or nil on
failure."
  (cond
    (*fake-effects*
     (push (cons program args) *fake-effects*)
     "")
    (t
     (handler-case
         (with-output-to-string (out)
           (let ((p (sb-ext:run-program program args :search t :output out :error nil)))
             (unless (zerop (sb-ext:process-exit-code p))
               (return-from run nil))))
       (error () nil)))))

(defparameter *user-agent* "voice-assistant/0.1.0"
  "Sent with every request, as the Rust side does. Some sites refuse curl's.")

(defn http-get ((url string) &key (header (or null string) nil) (timeout (integer 1) 3))
    (or null string)
  "The body at URL, or nil on any failure."
  (let ((body (apply #'run "curl" "-sfL" "-A" *user-agent* "--max-time" (princ-to-string timeout)
                     (append (and header (list "-H" header)) (list url)))))
    (and body (plusp (length body)) body)))

(defn spawn ((program string) &rest (args string)) null
  "Start PROGRAM with ARGS without waiting for it."
  (if *fake-effects*
      (push (cons program args) *fake-effects*)
      (handler-case (sb-ext:run-program program args :search t :wait nil
                                        :output nil :error nil :input nil)
        (error () nil)))
  nil)

(defn output ((program string) &rest (args string)) (or null string)
  "PROGRAM's trimmed output, nil when it fails or says nothing."
  (let ((out (apply #'run program args)))
    (and out (let ((s (string-trim '(#\Space #\Newline #\Tab) out))) (and (plusp (length s)) s)))))
