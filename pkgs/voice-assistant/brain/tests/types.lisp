;;; Every function the brain defines has a declared type.

(in-package #:va-tests)

(defun brain-functions ()
  "Functions whose home is the va package, without macros."
  (let (fs)
    (do-symbols (s :va fs)
      (when (and (eq (symbol-package s) (find-package :va)) (fboundp s)
                 (not (macro-function s)) (not (special-operator-p s)))
        (push s fs)))))

(defun struct-functions ()
  "What defstruct defines, typed by its slots already."
  (let (fs)
    (do-symbols (s :va fs)
      (let ((dd (and (eq (symbol-package s) (find-package :va))
                     (sb-kernel:find-defstruct-description s nil))))
        (when dd
          (push (sb-kernel:dd-predicate-name dd) fs)
          (push (find-symbol (format nil "COPY-~a" s) :va) fs)
          (dolist (c (sb-kernel:dd-constructors dd)) (push (car c) fs))
          (dolist (slot (sb-kernel:dd-slots dd))
            (push (sb-kernel:dsd-accessor-name slot) fs)))))))

(defun takes-anything-p (name)
  "Whether a parameter of NAME is declared as t."
  (let ((ty (sb-int:info :function :type name)))
    (and (typep ty 'sb-kernel:fun-type)
         (member sb-kernel:*universal-type*
                 (append (sb-kernel:fun-type-required ty) (sb-kernel:fun-type-optional ty)
                         (alexandria:ensure-list (sb-kernel:fun-type-rest ty))
                         (mapcar #'sb-kernel:key-info-type (sb-kernel:fun-type-keywords ty)))))))

(defparameter *takes-anything*
  '(va::car-safe va::choose va::pick va::command-action va::bad-template va::comma-p
    va::debug-value va::debug-plain va::info-weather
    va::reject va::untimed-p)
  "Functions that really take any value: format arguments, command fields and
Rust Debug printing.")

(define-test functions-are-typed
  :parent va-tests
  ;; Coalton types its constructors itself, Optional fields as t.
  (let* ((structs (append (struct-functions) (mapcar #'va:command-name va::*commands*)))
         (fs (remove-if (lambda (s) (member s structs)) (brain-functions))))
    (is equal '()
        (remove :declared fs :key (lambda (s) (sb-int:info :function :where-from s)))
        "functions without a declared type, define them with defn")
    (is equal '()
        (set-difference (remove-if-not #'takes-anything-p fs) *takes-anything*)
        "functions with a parameter of type t")))
