;;; What was asked, kept across restarts so "what was my last request"
;;; still has an answer days later.

(in-package #:va)

(defvar *history-file* nil
  "Where requests are kept, or nil for requests.tsv in the state directory.
Looked up when used, as the image is saved on a build machine.")

(defn history-path () pathname
  (or *history-file*
      (merge-pathnames "voice-assistant/requests.tsv"
                       (uiop:ensure-directory-pathname
                        (or (uiop:getenv-absolute-directory "XDG_STATE_HOME")
                            (merge-pathnames ".local/state/" (user-homedir-pathname)))))))

(defn record-request ((text string) (at integer)) null
  "Keep TEXT, asked at AT, as one line: seconds, a tab, the text."
  (let ((path (history-path)))
    (handler-case
        (progn (ensure-directories-exist path)
               (with-open-file (out path :direction :output :if-exists :append
                                    :if-does-not-exist :create)
                 (format out "~d~c~a~%" at #\Tab (substitute #\Space #\Newline text))))
      (file-error (e) (format *error-output* "~&va-brain: history: ~a~%" e))))
  nil)

(defn latest-request () (or null cons)
  "The last request kept as (at . text)."
  (let ((last nil))
    (handler-case
        (with-open-file (in (history-path) :if-does-not-exist nil)
          (when in
            (loop for line = (read-line in nil) while line
                  do (let ((tab (position #\Tab line)))
                       (when tab
                         (let ((at (parse-integer line :end tab :junk-allowed t)))
                           (when at (setf last (cons at (subseq line (1+ tab)))))))))))
      (file-error () nil))
    last))
