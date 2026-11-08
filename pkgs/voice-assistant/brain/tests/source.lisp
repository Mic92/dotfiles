;;; Rules about the source that once broke without anyone noticing.

(in-package #:va-tests)

(defun source-files (&rest dirs)
  "The lisp files in DIRS of the brain."
  (loop for d in dirs
        append (directory (merge-pathnames (make-pathname :directory `(:relative ,d)
                                                          :name :wild :type "lisp")
                                           (asdf:system-source-directory
                                            "voice-assistant-brain")))))

(defparameter *definition*
  (cl-ppcre:create-scanner
   (format nil "^\\((?:va::)?(~{~a~^|~}) \\(?(?:va::)?([^ ()]+)"
           '("defn" "defun" "defmacro" "defparameter" "defvar" "defstruct" "deftype"
             "defcommand" "defreplies" "defslot" "define-test"))
   :case-insensitive-mode t)
  "A top-level definition, capturing the definer and the name it defines.")

(defun namespace (definer)
  "Where a name DEFINER defines lives. A type and a function may share one."
  (cond ((member definer '("defparameter" "defvar" "defreplies") :test #'string-equal) :variable)
        ((member definer '("defstruct" "deftype") :test #'string-equal) :type)
        ((string-equal definer "define-test") :test)
        ((string-equal definer "defslot") :slot)
        (t :function)))

(defun top-level-names (file)
  "((namespace name) . file) for each top-level definition in FILE."
  (with-open-file (in file)
    (loop for line = (read-line in nil) while line
          for m = (nth-value 1 (cl-ppcre:scan-to-strings *definition* line))
          when m collect (cons (list (namespace (aref m 0)) (string-downcase (aref m 1)))
                               (pathname-name file)))))

(defun duplicates (files)
  "Names defined more than once in FILES, with where."
  (let ((seen (make-hash-table :test #'equal)))
    (dolist (f files) (dolist (d (top-level-names f)) (push (cdr d) (gethash (car d) seen))))
    (loop for name being the hash-keys of seen using (hash-value where)
          when (rest where) collect (cons name where))))

(define-test nothing-defined-twice
  :parent va-tests
  ;; A second definition silently replaces the first one, tests included.
  (is equal '() (duplicates (source-files "src" "skills")))
  (is equal '() (duplicates (source-files "tests"))))

(defun files-using (regex dirs)
  "Names of files in DIRS whose text matches REGEX."
  (loop for f in (apply #'source-files dirs)
        when (cl-ppcre:scan regex (uiop:read-file-string f))
        collect (pathname-name f)))

(define-test time-comes-from-the-context
  :parent va-tests
  ;; Skills answering from the wall clock pass or fail by the time of day.
  (is equal '("timer")
      (sort (files-using "get-(decoded|universal|internal-real)-time" '("src" "skills"))
            #'string<)))

(define-test http-goes-through-http-get
  :parent va-tests
  ;; It sets the user agent and timeout, and tests fake it.
  (is equal '("effects") (files-using "\"curl\"" '("src" "skills"))))

(define-test processes-go-through-effects
  :parent va-tests
  ;; Tests fake `run' and `spawn', not a program started behind their back.
  (is equal '("effects") (files-using "run-program" '("src" "skills"))))

(defun form-arg (head text)
  "The argument of the first (HEAD arg) in TEXT."
  (cl-ppcre:register-groups-bind (arg) ((format nil "\\(~a ([^)]+)\\)" head) text) arg))

(define-test skills-name-themselves
  :parent va-tests
  ;; Without in-skill, commands join the skill of the file loaded before.
  (dolist (f (source-files "skills"))
    (let ((text (uiop:read-file-string f)))
      (is equal (form-arg "defskill" text) (form-arg "in-skill" text) "~a" (pathname-name f)))))

(defun loaded-files (system)
  "The source files SYSTEM loads, through its modules."
  (labels ((walk (c)
             (if (typep c 'asdf:parent-component)
                 (mapcan #'walk (asdf:component-children c))
                 (list (asdf:component-pathname c)))))
    (walk (asdf:find-system system))))

(define-test every-file-is-loaded
  :parent va-tests
  ;; The system lists its files, one left out is silently never compiled.
  (is equal '() (set-difference (mapcar #'namestring (source-files "src" "skills"))
                                (mapcar #'namestring (loaded-files "voice-assistant-brain"))
                                :test #'string=))
  (is equal '() (set-difference (mapcar #'namestring (source-files "tests"))
                                (mapcar #'namestring (loaded-files "voice-assistant-brain/tests"))
                                :test #'string=)))

(define-test files-say-what-they-are-for
  :parent va-tests
  ;; A module whose name hides half of what it holds goes unnoticed without.
  (dolist (f (source-files "src" "skills"))
    (true (uiop:string-prefix-p ";;; " (uiop:read-file-string f)) "~a" (pathname-name f))))

(defun definitions (files)
  "(definer name) of each top-level definition in FILES."
  (loop for f in files
        append (with-open-file (in f)
                 (loop for line = (read-line in nil) while line
                       for m = (nth-value 1 (cl-ppcre:scan-to-strings *definition* line))
                       when m collect (list (string-downcase (aref m 0))
                                            (string-downcase (aref m 1)))))))

(defun defined-by (definer files)
  (loop for (d name) in (definitions files) when (string= d definer) collect name))

(define-test parameters-stay-constant
  :parent va-tests
  ;; State that changes is a defvar, a reload keeps it. Tests may bind
  ;; their own data.
  (let ((text (format nil "~{~a~%~}" (mapcar #'uiop:read-file-string
                                             (source-files "src" "skills")))))
    (dolist (name (defined-by "defparameter" (source-files "src" "skills")))
      (let ((q (cl-ppcre:quote-meta-chars name)))
        (false (cl-ppcre:scan (format nil "(?i)\\((setf|setq|push|pop|incf|decf)\\s+(?:va::)?~a[\\s)]" q)
                              text)
               "~a assigned" name)
        ;; Only a let binding puts it first in a list.
        (false (cl-ppcre:scan (format nil "(?i)\\(\\s*(?:va::)?~a\\s" q) text) "~a rebound" name)))))

(defun symbol-counts (files)
  "How often each symbol name occurs in FILES, package prefixes split off."
  (let ((counts (make-hash-table :test #'equal)))
    (dolist (f files counts)
      (dolist (tok (cl-ppcre:split "[^a-zA-Z0-9*+<>=/%!?-]+" (uiop:read-file-string f)))
        (incf (gethash (string-downcase tok) counts 0))))))

(define-test everything-defined-is-used
  :parent va-tests
  ;; Moved code leaves dead definitions behind. build.lisp calls the entry points.
  (let ((counts (symbol-counts (append (source-files "src" "skills" "tests")
                                       (directory (merge-pathnames
                                                   "*.lisp" (asdf:system-source-directory
                                                             "voice-assistant-brain"))))))
        (defs (loop for d in '("defn" "defun" "defmacro" "defparameter" "defvar")
                    append (defined-by d (source-files "src" "skills")))))
    (is equal '() (remove-if (lambda (name) (> (gethash name counts 0) 1)) defs))))
