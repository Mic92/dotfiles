;;; Reading parsed JSON (jzon: objects are hash tables, arrays vectors).

(in-package #:va)

(deftype json ()
  "A parsed JSON value. jzon reads null as the symbol null and false as nil."
  '(or hash-table vector string real boolean (eql null)))

(defn parse-json ((raw string)) (or null json)
  "RAW parsed, nil when it is not JSON."
  (handler-case (jzon:parse raw) (error () nil)))

(defn json-get ((v (or null json)) &rest (path (or string (integer 0)))) (or null json)
  "Follow keys and indices into parsed JSON, nil when missing."
  (dolist (p path v)
    (setf v (typecase p
              (string (and (hash-table-p v) (gethash p v)))
              (integer (and (vectorp v) (not (stringp v)) (< p (length v)) (aref v p)))
              (otherwise nil)))))

(defn json-int ((v (or null json)) (key string)) (or null integer)
  (let ((s (json-get v key))) (and (stringp s) (values (parse-integer s :junk-allowed t)))))
