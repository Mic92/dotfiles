;;; Reading parsed JSON (jzon: objects are hash tables, arrays vectors).

(in-package #:va)

(defn parse-json ((raw string)) t
  "RAW parsed, nil when it is not JSON."
  (handler-case (jzon:parse raw) (error () nil)))

(defn json-get ((v t) &rest (path t)) t
  "Follow keys and indices into parsed JSON, nil when missing."
  (dolist (p path v)
    (setf v (typecase p
              (string (and (hash-table-p v) (gethash p v)))
              (integer (and (vectorp v) (not (stringp v)) (< p (length v)) (aref v p)))
              (otherwise nil)))))

(defn json-int ((v t) (key string)) (or null integer)
  (let ((s (json-get v key))) (and (stringp s) (values (parse-integer s :junk-allowed t)))))
