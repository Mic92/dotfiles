(in-package #:va)

;;; A command: its templates, its Coalton constructor and how it runs.

(defstruct command
  (name nil :type symbol)
  ;; How Rust's Debug names it, and whether with field names.
  (kind "" :type string)
  (named nil :type boolean)
  ;; Printed without the skill around it, like Rust's core actions.
  (bare nil :type boolean)
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
  (run nil :type (or null function))
  ;; "and tomorrow?": the previous action and the words, to a new action.
  (refine nil :type (or null function)))

(declaim (ftype function question-act question-answer))

(defstruct (reply (:constructor reply (title body speech &key live question)))
  "What the assistant says, and the notification showing it. LIVE replies
are not cached for \"repeat that\"."
  (title "" :type string)
  (body "" :type string)
  (speech "" :type string)
  (live nil :type boolean)
  ;; How to take the next utterance.
  (question nil :type (or null question)))

(defstruct (question (:constructor question (&key answer act)))
  "A question we asked and wait to have answered: ANSWER takes free text
(\"the computer\", \"7 am\"), ACT yes, no and the like. Either returns an
action, or nil to fall through."
  (answer nil :type (or null function))
  (act nil :type (or null function)))

(defvar *commands* nil
  "Every command, by skill in `*skill-order*`, then in definition order: ties
go to the earlier one.")

(defparameter *skill-order*
  '(timer system volume media desktop apps info calc spell fun chat ops notes reminders
    meetings news kagi core))

(defn add-command ((c command)) symbol
  (let ((old (member (command-name c) *commands* :key #'command-name)))
    (if old
        (setf (car old) c)
        (setf *commands*
              (stable-sort (append *commands* (list c)) #'<
                           :key (lambda (c) (or (position (command-skill c) *skill-order*)
                                                (length *skill-order*))))))
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

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defn car-safe ((x t)) t (and (consp x) (car x)))

  (defn join-template ((parts list)) string
    "Pieces of a long template. Spaces inside groups are harmless."
    (format nil "~{~a~^ ~}" parts))

  (defn default-kind ((name symbol) (skill symbol)) string
    "NAME in CamelCase, without the skill's prefix: timer-cancel is Cancel."
    (let* ((s (symbol-name name))
           (prefix (format nil "~a-" skill))
           (s (if (uiop:string-prefix-p prefix s) (subseq s (length prefix)) s)))
      (remove #\- (string-capitalize s)))))

(defmacro defcommand (name &body body)
  "(defcommand NAME TEMPLATE... [:key value]... FORM...)
A template is a string, or (:join string...) for long ones.
Keys: :kind, :named, :bare, :refine, :slots ((var CoaltonType)...), :parse,
:filler, :repeatable, :fallback, :examples. :parse turns the matched slots, read with
`(slot name [default])`, into the fields: a list, t for none, or nil to
reject the match. It sees the matched template's source as TEMPLATE.
Without it slot names are the field names. Fields are bound as variables
in FORMs."
  (let* ((templates (loop while (or (stringp (car body)) (eq (car-safe (car body)) :join))
                          collect (let ((tpl (pop body)))
                                    (if (stringp tpl) tpl (join-template (rest tpl))))))
         (keys (loop while (keywordp (car body)) append (list (pop body) (pop body))))
         (slots (getf keys :slots)))
    `(progn
       (add-command
        (make-command :name ',name
                      :kind ,(or (getf keys :kind) (default-kind name *skill*))
                      :named ,(getf keys :named)
                      :bare ,(getf keys :bare)
                      :refine ,(getf keys :refine)
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
