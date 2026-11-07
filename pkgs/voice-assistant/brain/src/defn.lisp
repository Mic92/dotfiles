(in-package #:va)

(defmacro defn (name params result &body body)
  "A function with its types inline:
  (defn name ((x string) &optional (n fixnum 0) &rest (more string)) result body...)
declaims its ftype, then defines it."
  (let (types args (mode :required))
    (dolist (p params)
      (cond
        ((member p '(&optional &rest &key))
         (setf mode p) (push p types) (push p args))
        (t
         (destructuring-bind (var type &optional (default nil defaultp)) p
           (push (if (eq mode '&key) (list (intern (symbol-name var) :keyword) type) type) types)
           (push (if defaultp (list var default) var) args)))))
    `(progn
       (declaim (ftype (function ,(nreverse types)
                                 ,(if (and (consp result) (eq (car result) 'values))
                                      `(values ,@(cdr result) &optional)
                                      `(values ,result &optional)))
                       ,name))
       (defun ,name ,(nreverse args) ,@body))))

(defmacro in (word &rest choices)
  "Whether WORD is one of the strings CHOICES."
  `(and (member ,word ',choices :test #'equal) t))

(defmacro word-case (word &body clauses)
  "`case` for strings: each clause is (\"key\" form...), ((\"key\"...) form...)
or (t form...)."
  (let ((w (gensym "W")))
    `(let ((,w ,word))
       (cond ,@(loop for (keys . body) in clauses
                     collect (if (eq keys t)
                                 `(t ,@body)
                                 `((member ,w ',(alexandria:ensure-list keys) :test #'equal)
                                   ,@body)))))))
