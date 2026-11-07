(in-package #:va)

;;; A command: its templates, its Coalton constructor and how it runs.
;;; Matching templates comes with the grammar port.

(defstruct command
  (name nil :type symbol)
  (skill nil :type symbol)
  (templates nil :type list)
  (examples nil :type list)
  (filler nil)
  (repeatable nil :type boolean)
  (fields nil :type list)
  (run nil :type (or null function)))

(defvar *commands* (make-hash-table :test #'eq)
  "Every command by name.")

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *skill-commands* (make-hash-table :test #'eq)
    "Per skill, (name (slot type)...) for each command in definition order.")
  (defvar *skill* nil "The skill being defined, set by `in-skill`."))

(declaim (ftype (function () (values list &optional)) commands))
(defun commands () (loop for c being the hash-values of *commands* collect c))

(defmacro defcommand (name &body body)
  "(defcommand NAME TEMPLATE... [:key value]... FORM...)
Keys: :slots ((var CoaltonType)...), :filler, :repeatable, :examples.
Slots are bound as variables in FORMs."
  (let* ((templates (loop while (stringp (car body)) collect (pop body)))
         (keys (loop while (keywordp (car body)) append (list (pop body) (pop body))))
         (slots (getf keys :slots)))
    `(progn
       (setf (gethash ',name *commands*)
             (make-command :name ',name
                           :skill *skill*
                           :templates ',templates
                           :examples ',(getf keys :examples)
                           :filler ,(getf keys :filler)
                           :repeatable ,(getf keys :repeatable)
                           :fields ',slots
                           :run (lambda ,(mapcar #'car slots)
                                  (declare (ignorable ,@(mapcar #'car slots)))
                                  ,@body)))
       ;; At compile time too, so `defskill` can see it.
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (setf (gethash *skill* *skill-commands*)
               (append (remove ',name (gethash *skill* *skill-commands*) :key #'car)
                       (list '(,name ,@slots)))))
       ',name)))

(defmacro in-skill (name)
  "Start a skill file: later commands belong to NAME."
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf *skill* ',name)
     (setf (gethash ',name *skill-commands*) nil)))

(defmacro defskill (name)
  "End a skill file: the Coalton type with a constructor per command."
  (let ((ctors (loop for (c . slots) in (gethash name *skill-commands*)
                     collect `(,(intern (symbol-name c))
                                ,@(mapcar #'second slots)))))
    `(coalton:coalton-toplevel
      (coalton:define-type ,(intern (symbol-name name)) ,@ctors))))
