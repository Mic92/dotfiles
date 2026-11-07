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
  (is equal '("dispatch" "timer")
      (sort (files-using "get-(decoded|universal|internal-real)-time" '("src" "skills"))
            #'string<)))

(define-test http-goes-through-http-get
  :parent va-tests
  ;; It sets the user agent and timeout, and tests fake it.
  (is equal '("effects") (files-using "\"curl\"" '("src" "skills"))))
