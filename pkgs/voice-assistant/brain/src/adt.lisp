(in-package #:va)

;;; Taking Coalton values apart from plain Lisp.

(define-condition no-constructor (error)
  ((type :initarg :type) (name :initarg :name))
  (:report (lambda (c s) (format s "no constructor ~a of ~a"
                                 (slot-value c 'name) (slot-value c 'type)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defn field ((i (integer 0))) symbol
    "The slot holding a Coalton constructor's I-th field."
    (values (find-symbol (format nil "_~d" i) '#:va.types))))

(defmacro adt-case (value type &body clauses)
  "Dispatch on the constructor of VALUE, a Coalton TYPE (in VA.TYPES).
Each clause is ((Constructor var...) form...) or (t form...)."
  (let ((v (gensym "V")))
    `(let ((,v ,value))
       (cond
         ,@(loop for (pattern . body) in clauses
                 collect
                 (if (eq pattern t)
                     `(t ,@body)
                     (destructuring-bind (ctor &rest vars) pattern
                       (let ((class (find-symbol (format nil "~a/~a" (symbol-name type)
                                                         (symbol-name ctor))
                                                 '#:va.types)))
                         (unless class
                           (error 'no-constructor :type type :name ctor))
                         `((typep ,v ',class)
                           (let ,(loop for var in vars
                                       for i from 0
                                       collect `(,var (slot-value ,v ',(field i))))
                             (declare (ignorable ,@vars))
                             ,@body))))))))))
