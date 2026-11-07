(defpackage #:va-tests
  (:documentation "The brain's tests, many ported from the Rust ones.")
  (:use #:cl #:parachute))
(in-package #:va-tests)

(define-test va-tests)

(va:defreplies *lines* "a" "b" "c" "d" "e" "f")
(va:defreplies *greeting* "Hi." (:late "Shh, it's late."))

(define-test pick-varies
  :parent va-tests
  (let ((said (loop repeat 30 collect (va:pick *lines*))))
    (loop for (x y z) on said while z
          do (false (or (equal x y) (equal x z) (equal y z)) "~s" said))))

(define-test late-lines
  :parent va-tests
  (is equal "Hi." (va:pick *greeting*))
  (is equal "Shh, it's late." (let ((va:*late* t)) (va:pick *greeting*))))

(define-test effects-are-recorded
  :parent va-tests
  (is equal '(("notify-send" "Reminder" "tea"))
      (va:fake-effects (va:run "notify-send" "Reminder" "tea"))))

(define-test commands-have-examples
  :parent va-tests
  (dolist (c (va:commands))
    (when (va:command-templates c)
      (true (va:command-examples c) "~a has no examples" (va:command-name c)))))

(define-test skill-types-exist
  :parent va-tests
  (true (fboundp 'va::dice))
  (true (find-class 'va::fun nil)))

(define-test every-function-is-typed
  :parent va-tests
  (let (untyped)
    (do-symbols (s '#:va)
      (when (and (eq (symbol-package s) (find-package '#:va))
                 (fboundp s)
                 (not (macro-function s))
                 (not (typep (fdefinition s) 'generic-function))
                 ;; defstruct and Coalton declare their own.
                 (not (eq (sb-int:info :function :where-from s) :declared))
                 (not (sb-int:info :function :source-transform s)))
        (push s untyped)))
    (is equal nil untyped)))
