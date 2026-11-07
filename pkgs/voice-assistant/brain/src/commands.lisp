(in-package #:va)

;;; A command: its templates, its Coalton constructor and how it runs.

(defstruct command
  (name nil :type symbol)
  ;; How Rust's Debug names it.
  (kind "" :type string)
  (skill nil :type symbol)
  (templates nil :type list)
  (parsed nil :type list)
  (examples nil :type list)
  (filler nil)
  (repeatable nil :type boolean)
  (fallback nil :type boolean)
  (fields nil :type list)
  ;; Slots to field values, or nil to reject the match and t to accept it without fields.
  (parse nil :type (or null function))
  (run nil :type (or null function)))

(defstruct (reply (:constructor reply (title body speech &key live)))
  "What the assistant says, and the notification showing it. LIVE replies
are not cached for \"repeat that\"."
  (title "" :type string)
  (body "" :type string)
  (speech "" :type string)
  (live nil :type boolean))

(defvar *commands* nil
  "Every command, in definition order: ties go to the earlier one.")

(defn add-command ((c command)) symbol
  (let ((old (member (command-name c) *commands* :key #'command-name)))
    (if old
        (setf (car old) c)
        (setf *commands* (append *commands* (list c))))
    (command-name c)))

(defmacro with-slots-of (slots &body body)
  "BODY with `(slot name [default])` reading the alist SLOTS."
  (let ((s (gensym "SLOTS")))
    `(let ((,s ,slots))
       (flet ((slot (name &optional default)
                (let ((cell (assoc name ,s :test #'string=)))
                  (if cell (cdr cell) default))))
         (declare (ignorable #'slot))
         ,@body))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *skill-commands* (make-hash-table :test #'eq)
    "Per skill, (name (slot type)...) for each command in definition order.")
  (defvar *skill* nil "The skill being defined, set by `in-skill`."))

(defn commands () list *commands*)

(defmacro defcommand (name &body body)
  "(defcommand NAME TEMPLATE... [:key value]... FORM...)
Keys: :kind, :slots ((var CoaltonType)...), :parse, :filler, :repeatable,
:fallback, :examples. :parse turns the matched slots, read with
`(slot name [default])`, into the fields: a list, t for none, or nil to
reject the match. It sees the matched template's source as TEMPLATE.
Without it slot names are the field names. Fields are bound as variables
in FORMs."
  (let* ((templates (loop while (stringp (car body)) collect (pop body)))
         (keys (loop while (keywordp (car body)) append (list (pop body) (pop body))))
         (slots (getf keys :slots)))
    `(progn
       (add-command
        (make-command :name ',name
                      :kind ,(or (getf keys :kind) (remove #\- (string-capitalize name)))
                      :skill *skill*
                      :templates ',templates
                      :parsed (mapcar #'parse-template ',templates)
                      :examples ',(getf keys :examples)
                      :filler ,(getf keys :filler)
                      :repeatable ,(getf keys :repeatable)
                      :fallback ,(getf keys :fallback)
                      :fields ',slots
                      :parse (lambda (slots template)
                               (declare (ignorable template))
                               (with-slots-of slots
                                 ,(or (getf keys :parse)
                                      (if slots
                                          `(list ,@(loop for (var) in slots
                                                         collect `(slot ,(string-downcase var))))
                                          t))))
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
