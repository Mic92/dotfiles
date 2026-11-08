;;; Words that mean nothing on their own ("stop", "nah", "again", "and
;;; tomorrow?") are parsed once into dialogue acts, then handed to what the
;;; user may be referring to, most salient first: the question we asked,
;;; the last action, a timer, the music.
;;;
;;; Acts are :yes, :no (also "never mind"), :maybe, :stop, :cancel, :pause,
;;; :resume, :again and (:refine words) for "and tomorrow?".

(in-package #:va)

(defparameter *act-rules*
  (mapcar (lambda (rule) (cons (mapcar #'parse-template (car rule)) (cdr rule)))
          `((("!stop [it|that|this] [now|please|immediately|right now]"
              "![can you|could you|please] stop [it|that|this] [please]"
              "!stop just stop"
              "!stop (what you're doing|what you are doing|doing that|doing this)"
              "!(halt|quit|terminate) [it|that]")
             . :stop)
            (("![oops|actually|ai] (cancel|abort) [it|that|this] [now]"
              "![can you|could you|please] (cancel|abort) [it|that|this] [please]"
              ,(join-template '("!(cancel|abort) [that|the|my] [last|previous|current]"
                                "(thing|request|command|input|task|action)"))
              "!(scrap|scratch) [it|that]"
              "!abort mission"
              "!command cancel"
              "!i repeat cancel")
             . :cancel)
            (("!(pause|pause it|pause that)") . :pause)
            (("!(resume|unpause|continue|resume it|unpause it|continue it)") . :resume)
            (("!never mind [don't do that|cancel that]"
              "!nevermind [cancel that]"
              ,(join-template '("![actually] forget (it|that|about it)"
                                "[i don't need it|i do not need it anymore]"))
              "!forget (my previous command|what i told you to do|what i said)"
              "!ignore (the|my) (previous|last) command"
              "!(don't|do not) do that"
              "![stop] i (changed|have changed|'ve changed) my mind"
              "!nothing")
             . :no)
            ((,(join-template '("![tell|give|say|do] [me|us] (again|once more|one more"
                                "|one more time|another|another one|and another|do it again"
                                "|same again|more|even more|more please|keep going|a bit more"
                                "|bit more)")))
             . :again)
            ((,(join-template '("!(and|also|plus|what about|how about|and what about"
                                "|and how about|same for|now|and now) {text}")))
             . :refine)))
  "Templates for each act.")

(defn refine-words ((text string)) list
  "\"and in munich\", \"what about at 3\": the words for one slot."
  (let ((ws (uiop:split-string text)))
    (if (and (rest ws) (in (first ws) "in" "at" "for" "on")) (rest ws) ws)))

(deftype act ()
  "What a whole utterance does besides a command, like :stop or (:refine words)."
  '(or keyword (cons (eql :refine) (cons list null))))

(defn dialogue-act ((ws words)) (or null act)
  "The act a whole utterance is, if any."
  (let (best best-skipped)
    (loop for (templates . act) in *act-rules*
          do (dolist (tpl templates)
               (multiple-value-bind (skipped slots) (find-template tpl ws #'slot-parser)
                 (when (and skipped (or (null best) (< skipped best-skipped)))
                   (setf best (if (eq act :refine)
                                  (list :refine (refine-words (cdr (assoc "text" slots
                                                                          :test #'string=))))
                                  act)
                         best-skipped skipped)))))
    best))

(defreplies *not-again*
  "Again? Once was plenty, darling."
  "No encores for that one. I have standards."
  "Do it again? What am I, your intern?"
  "Encore? Pay me first.")

(defreplies *lost*
  "Another what? I've lost the thread, babe."
  "Again? We haven't even started."
  "More of what? Use your words."
  "Another one of what? Help me out here, I'm only software.")

(defreplies *unsure*
  "No rush. Ask me again when you've made up your mind."
  "Take your time. I'll forget about it in the meantime."
  "Indecisive. Cute. Let's drop it for now.")

(defn command-action ((name symbol) &rest (fields t)) (or null action)
  "An action of command NAME, nil while it is not ported."
  (let ((c (find name *commands* :key #'command-name)))
    (and c (make-action c fields))))

(defn say-action ((text string)) (or null action) (command-action 'say text))

(defn ambient ((act act)) (or null action)
  "An act without a question, resolved against the rest of the focus.
\"never mind\" never touches a timer or the music."
  (let ((previous (context-previous *context*))
        (timer (context-timer *context*))
        (music (context-music *context*)))
    (trivia:match act
      (:again (cond ((null previous) (say-action (pick *lost*)))
                    ((command-repeatable (action-command previous)) previous)
                    (t (say-action (pick *not-again*)))))
      ((list :refine ws)
       (let ((refine (and previous (command-refine (action-command previous)))))
         (and refine (funcall refine previous ws))))
      ((or :stop :cancel)
       (cond (timer (command-action 'timer-cancel))
             ((and music (eq act :stop)) (command-action 'media-stop))
             (t (command-action 'dismiss))))
      ((or :pause :resume) (cond (timer (command-action 'timer-pause))
                                 ((eq act :pause) (command-action 'media-pause))
                                 (t (command-action 'media-play))))
      (:no (command-action 'dismiss)))))

(defn confirm ((yes (or null action)) (no (or null action))) question
  "\"Shall I?\": one action for yes, another for no."
  (question :act (lambda (act) (case act (:yes yes) ((:no :cancel :stop) no) (otherwise nil)))))

(defn respond ((text string) (q (or null question))) (or null action)
  "What the user meant: the question's answer first, then a full command,
then an act about whatever else is going on."
  (unless q (return-from respond (parse-tier text nil)))
  (let* ((ws (normalize text))
         (act (dialogue-act ws))
         ;; "never mind" is no answer, even to "what shall I remind you of?".
         (firm (member act '(:stop :cancel :no))))
    (or (and (not firm) (question-answer q) (funcall (question-answer q) text))
        ;; Loose yes/no words only when the sentence is no command of its own.
        (and (null act) (parse-tier text nil))
        (let ((act (or act (answer-word ws))))
          (and act (act-response act q text))))))

(defn act-response ((act act) (q question) (text string)) (or null action)
  "What a bare act like \"no\" or \"maybe\" means: the question's say first."
  (or (and (question-act q) (funcall (question-act q) act))
      (case act
        ((:stop :cancel :no) (command-action 'dismiss))
        (:maybe (say-action (pick *unsure*)))
        (otherwise (parse-tier text nil)))))
