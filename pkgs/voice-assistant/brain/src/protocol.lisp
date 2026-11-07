;;; The protocol the Rust ears and mouth speak: one JSON object per line,
;;; each answered by one object with the same id.

(in-package #:va)

(defconstant +protocol-version+ 1
  "Bumped on any incompatible message change, checked by hello.")

(defvar *send* nil
  "Called with a message to send before the reply, such as a filler line.")

(define-condition bad-message (simple-error) ()
  (:documentation "A request that can't be answered, told to the sender."))

(defn reject ((control string) &rest (args t)) nil
  (error 'bad-message :format-control control :format-arguments args))

(defn message-field ((msg hash-table) (key string) (type (or symbol cons))) json
  "KEY of MSG, which must be of TYPE."
  (multiple-value-bind (v found) (gethash key msg)
    (cond ((not found) (reject "field ~s is missing" key))
          ((typep v type) v)
          (t (reject "field ~s must be ~a" key
                     (cond ((subtypep type 'integer) "an integer")
                           ((subtypep type 'string) "a string")
                           ((subtypep type 'vector) "an array")
                           (t (format nil "of type ~(~a~)" type))))))))

(defn object (&rest (pairs json)) hash-table
  "A JSON object of alternating keys and values, in that order."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (k v) on pairs by #'cddr do (setf (gethash k h) v))
    h))

(defn message-context ((msg hash-table)) context
  (make-context (message-field msg "now" '(integer 0 86400)) (message-field msg "today" 'integer)))

(defn examples-reply ((msg hash-table)) hash-table
  (let ((*context* (message-context msg)))
    (object "type" "examples"
            "examples" (map 'vector (lambda (e)
                                      (destructuring-bind (text kind slotted) e
                                        (object "text" text "kind" kind "slotted" slotted)))
                            (classifier-examples)))))

(defn classified-reply ((msg hash-table)) hash-table
  (let ((*context* (message-context msg)))
    (classified (message-field msg "text" 'string) (message-field msg "kind" 'string)
                (message-field msg "score" 'real) (message-field msg "lead" 'real))))

(defn parses ((msg hash-table)) hash-table
  (let ((*context* (message-context msg)))
    (object "type" "parsed"
            "parses" (map 'vector
                          (lambda (text)
                            (unless (stringp text) (reject "field \"texts\" must hold strings"))
                            (flet ((shown (fallback)
                                     (let ((a (parse-tier text fallback)))
                                       (if a (action-debug a) 'null))))
                              (object "strict" (shown nil) "fallback" (shown t))))
                          (message-field msg "texts" 'vector)))))

(defn answer-body ((msg hash-table)) hash-table
  (let ((type (message-field msg "type" 'string)))
    (cond ((string= type "hello") (object "type" "hello" "version" +protocol-version+))
          ((string= type "parse") (parses msg))
          ((string= type "heard") (hear msg))
          ((string= type "examples") (examples-reply msg))
          ((string= type "classified") (classified-reply msg))
          ((string= type "lexicon") (object "type" "lexicon" "sentences" (lexicon)))
          (t (reject "unknown message type ~s" type)))))

(defn answer ((msg hash-table)) hash-table
  "The reply to MSG, carrying its id. Bad requests get an error reply. Messages
sent before it carry the id too."
  (let* ((id (gethash "id" msg 'null))
         (send *send*)
         (*send* (and send (lambda (m) (setf (gethash "id" m) id) (funcall send m))))
         (body (handler-case (answer-body msg)
                 (bad-message (c) (object "type" "error" "message" (princ-to-string c)))))
         (reply (object "id" id)))
    (maphash (lambda (k v) (setf (gethash k reply) v)) body)
    reply))

(defn answer-line ((line string)) string
  (let ((msg (parse-json line)))
    (jzon:stringify (if (hash-table-p msg)
                        (answer msg)
                        (object "id" 'null "type" "error" "message" "not a JSON object")))))

(defn converse ((stream stream)) null
  "Answer each request line on STREAM until the other side hangs up."
  (let ((*send* (lambda (m) (write-line (jzon:stringify m) stream) (finish-output stream))))
    (loop for line = (read-line stream nil)
          while line
          do (write-line (answer-line line) stream)
          (finish-output stream))))

(defn listening-socket ((path pathname)) sb-bsd-sockets:local-socket
  "A fresh Unix socket listening on PATH."
  (ensure-directories-exist path)
  (when (probe-file path) (delete-file path))
  (let ((server (make-instance 'sb-bsd-sockets:local-socket :type :stream)))
    (sb-bsd-sockets:socket-bind server (namestring path))
    (sb-bsd-sockets:socket-listen server 8)
    server))

(defn handle-client ((client sb-bsd-sockets:local-socket) (name string) (handler function))
    null
  "Call HANDLER with a stream for CLIENT, then hang up."
  (unwind-protect
       (handler-case
           (funcall handler (sb-bsd-sockets:socket-make-stream
                             client :input t :output t :element-type 'character
                             :external-format :utf-8))
         (error (c) (format *error-output* "~&va-brain: ~a: ~a~%" name c)))
    (sb-bsd-sockets:socket-close client))
  nil)

(defn accept-on ((path pathname) (name string) (handler function)) nil
  "Call HANDLER with a stream for each connection to the Unix socket PATH,
each in its own thread."
  (serve-socket (listening-socket path) name handler))

(defn serve-socket ((server sb-bsd-sockets:local-socket) (name string) (handler function)) nil
  (loop
   (let ((client (sb-bsd-sockets:socket-accept server)))
     (sb-thread:make-thread (lambda () (handle-client client name handler)) :name name))))

(defn serve-one ((path pathname) (handler function)) null
  "Serve a single client on PATH, as a private brain does, which then has
no reason to live on, even when its parent was killed without cleaning up."
  (let ((server (listening-socket path)))
    (unwind-protect
         (handle-client (sb-bsd-sockets:socket-accept server) "va-private" handler)
      (sb-bsd-sockets:socket-close server)
      (when (probe-file path) (delete-file path)))
    nil))
