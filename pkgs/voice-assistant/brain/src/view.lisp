;;; Views: what a reply shows on screen beside what it says, drawn by the
;;; noctalia plugin mic92/voice-assistant.

(in-package #:va)

(defn obj (&rest (pairs t)) hash-table
  "A JSON object from KEY VALUE..., leaving out nil values."
  (let ((h (make-hash-table :test #'equal)))
    (loop for (k v) on pairs by #'cddr
          when v do (setf (gethash k h) v))
    h))

(defn show-view ((view hash-table)) null
  "Hand VIEW to the panel. Through a file: a route is too long for one
argument, and IPC payloads are re-split on spaces."
  (if *fake-effects*
      (push (list :view view) *fake-effects*)
      (let ((path (merge-pathnames "view.json" (runtime-dir))))
        (handler-case
            (progn (ensure-directories-exist path)
                   (with-open-file (out path :direction :output :if-exists :supersede)
                     (jzon:stringify view :stream out))
                   (spawn "noctalia" "msg" "plugin" "mic92/voice-assistant:service" "all" "show"
                          (namestring path)))
          (file-error (e) (format *error-output* "~&va-brain: view: ~a~%" e)))))
  nil)

(defn viewed ((view hash-table) (title string) (body string) (speech string)) reply
  "A live reply with VIEW on screen in place of its notification."
  (show-view view)
  (reply title body speech :live t :shown t))
