(in-package #:va)

(defvar *late* nil
  "Whether it is late at night, 23:00 to 5:00. Bound per request.")

(defstruct replies (lines #() :type vector) (late #() :type vector))

(defmacro defreplies (name &body lines)
  "Lines to pick from. A trailing (:late ...) adds late-night lines."
  (let ((late (cdr (find-if (lambda (l) (and (consp l) (eq (car l) :late))) lines)))
        (day (remove-if #'consp lines)))
    `(defparameter ,name (make-replies :lines ,(coerce day 'vector)
                                       :late ,(coerce late 'vector)))))

(defvar *recent* (make-hash-table :test #'eq :synchronized t)
  "Per reply table, the indices said lately.")

(declaim (ftype (function (replies &rest t) (values string &optional)) pick))
(defun pick (replies &rest args)
  "A line from REPLIES formatted with ARGS, none of the last few again.
Late lines replace the others at night."
  (let* ((lines (if (and *late* (plusp (length (replies-late replies))))
                    (replies-late replies)
                    (replies-lines replies)))
         (n (length lines))
         (seen (gethash lines *recent*))
         (fresh (loop for i below n unless (member i seen) collect i))
         (i (nth (random (length fresh)) fresh)))
    (setf (gethash lines *recent*) (subseq (cons i seen) 0 (min (floor n 2) (1+ (length seen)))))
    (apply #'format nil (aref lines i) args)))
