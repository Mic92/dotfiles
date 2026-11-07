;;; The messages in tests/protocol/, which the Rust tests decode too.

(in-package #:va-tests)

(defun fixture (name)
  (let ((file (format nil "../tests/protocol/~a.json" name)))
    (com.inuoe.jzon:parse
     (uiop:read-file-string (asdf:system-relative-pathname "voice-assistant-brain" file)))))

(defun canonical (json)
  "JSON with object keys sorted, so key order does not matter."
  (typecase json
    (hash-table (sort (loop for k being the hash-keys of json using (hash-value v)
                            collect (cons k (canonical v)))
                      #'string< :key #'car))
    (string json)
    (vector (map 'list #'canonical json))
    (otherwise json)))

(define-test protocol-fixtures
  :parent va-tests
  (dolist (name '("hello" "parse" "unknown" "bad-field" "heard"))
    (let ((f (fixture name)))
      (is equal (canonical (gethash "reply" f))
          (canonical (va::answer (gethash "request" f)))
          "~a" name))))

(define-test protocol-lines
  :parent va-tests
  (is equal "{\"id\":null,\"type\":\"error\",\"message\":\"not a JSON object\"}"
      (va::answer-line "[1,")))
