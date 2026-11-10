;;; Every side effect goes through `run`, so dev brains and tests only
;;; record what would have happened.

(in-package #:va)

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

(defn effects () list *fake-effects*)

(defn run ((program string) &rest (args string)) (or null string)
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

(defn http-get ((url string) &key (header (or null string) nil) (timeout (integer 1) 3)
                (ip (member nil :v4 :v6) nil))
    (or null string)
  "The body at URL, or nil on any failure. IP forces IPv4 or IPv6."
  (let ((body (apply #'run "curl" "-sfL" "-A" *user-agent* "--max-time" (princ-to-string timeout)
                     (append (and header (list "-H" header))
                             (case ip (:v4 '("-4")) (:v6 '("-6")) (otherwise nil))
                             (list url)))))
    (and body (plusp (length body)) body)))

(defn spawn ((program string) &rest (args string)) null
  "Start PROGRAM with ARGS without waiting for it."
  (if *fake-effects*
      (push (cons program args) *fake-effects*)
      (handler-case (sb-ext:run-program program args :search t :wait nil
                                        :output nil :error nil :input nil)
        (error () nil)))
  nil)

(defn launch ((program string) &rest (args string)) null
  "Start PROGRAM in its own scope, so it survives the brain restarting."
  (apply #'spawn "systemd-run" "--user" "--scope" "--quiet" "--slice=app.slice" program args))

(defn run-later ((seconds (integer 0)) (program string) &rest (args string)) null
  "PROGRAM with ARGS after SECONDS, from a systemd timer rather than a
sleeping child of ours."
  (apply #'spawn "systemd-run" "--user" "--quiet" "--collect"
         (format nil "--on-active=~d" seconds) program args))

(defn output ((program string) &rest (args string)) (or null string)
  "PROGRAM's trimmed output, nil when it fails or says nothing."
  (let ((out (apply #'run program args)))
    (and out (let ((s (trim out))) (and (plusp (length s)) s)))))

(defn read-proc ((path string)) string
  "A /proc file's text, empty where there is none."
  (handler-case (uiop:read-file-string path) (file-error () "")))

(defn append-line ((path pathname) (line string)) boolean
  "Append LINE to the file at PATH, creating both. Whether that worked."
  (cond (*fake-effects* (push (list :append (namestring path) line) *fake-effects*) t)
        (t (handler-case
               (progn (ensure-directories-exist path)
                      (with-open-file (out path :direction :output :if-exists :append
                                           :if-does-not-exist :create)
                        (write-line line out))
                      t)
             (file-error () nil)))))
