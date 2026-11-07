;;; The messages in tests/protocol/, which the Rust tests decode too.

(in-package #:va-tests)

(defun fixture (name)
  (let ((file (format nil "../tests/protocol/~a.json" name)))
    (com.inuoe.jzon:parse
     (uiop:read-file-string (asdf:system-relative-pathname "voice-assistant-brain" file)))))

(define-test protocol-fixtures
  :parent va-tests
  (dolist (name '("hello" "parse" "unknown" "bad-field"))
    (let ((f (fixture name)))
      (is equal (com.inuoe.jzon:stringify (gethash "reply" f))
          (com.inuoe.jzon:stringify (va::answer (gethash "request" f)))
          "~a" name))))

(define-test protocol-lines
  :parent va-tests
  (is equal "{\"id\":null,\"type\":\"error\",\"message\":\"not a JSON object\"}"
      (va::answer-line "[1,")))
