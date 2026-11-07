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
