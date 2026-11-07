;;; Todo and shopping lists in todoman, and quick notes.

(in-package #:va)

(in-skill notes)

(defparameter *lists*
  '(("Personal" "todo" "to do" "task" "tasks" "personal"
     "things to do" "pending tasks" "chores" "chore")
    ("Shopping List (Shannan Lekwati)" "shopping" "grocery" "groceries" "shopping and grocery")
    ("Movies and TV (Shannan Lekwati)" "movie" "movies" "film" "films" "tv" "watch")
    ("Restaurants To Visit (Shannan Lekwati)" "restaurant" "restaurants")
    ("Geschenke" "gift" "gifts" "present" "presents")
    ("Places to go (Shannan Lekwati)" "places" "travel"))
  "todoman lists and their spoken names, the first one what we call it. Commands
carry indices, 0 is the todo list and 1 shopping.")

(defparameter *compounds*
  '("salt and pepper" "mac and cheese" "fish and chips" "peanut butter and jelly"
    "sweet and sour" "salt and vinegar" "oil and vinegar" "bread and butter")
  "Items whose and is part of them.")

(defparameter *item-fillers*
  '("some" "a" "an" "the" "more" "few" "any" "also" "of" "new" "chore" "task")
  "Leading words that are not part of an item, as in \"the chore of vacuuming\".")

(defparameter *instructions* '("add" "put" "remind" "need" "so" "forget" "please")
  "Words starting what follows an item, as in \"milk so add that to my list\".")

(defn list-lookup ((ws words)) (or null (integer 0))
  (let ((name (unwords ws)))
    (position-if (lambda (l) (member name (rest l) :test #'string=)) *lists*)))

(defslot "list" (ws next) (list-lookup ws))
(closed-slot "list" (lambda () (loop for l in *lists* append (mapcar #'split-words (rest l)))))
;; Unlike {text}, commas stay: they separate items.
(defslot "items" (ws next) (and ws (unwords ws)))

(defn same-item ((a string) (b string)) boolean
  "\"Milk \" and \"milk\" are the same thing, so are \"egg\" and \"eggs\"."
  (flet ((stem (s) (cl-ppcre:regex-replace "e?s$" (string-downcase (trim s)) "")))
    (string= (stem a) (stem b))))

(defn instruction-p ((w string)) boolean
  (and (member w *instructions* :test #'string=) t))

(defn split-items ((text string)) list
  "\"some milk, eggs and a loaf of bread\" -> (\"milk\" \"eggs\" \"loaf of bread\")."
  (let* ((text (cl-ppcre:regex-replace " but .*" text ""))
         (text (cl-ppcre:regex-replace-all " (?:anymore|any more)\\b" text ""))
         (text (cl-ppcre:regex-replace-all "[,;]" text " and ")))
    (dolist (c *compounds*)
      (setf text (cl-ppcre:regex-replace-all c text (substitute #\Bel #\Space c))))
    (let (items)
      (dolist (chunk (cl-ppcre:split " and | & " text) (nreverse items))
        (let* ((ws (member-if-not (lambda (w) (member w *item-fillers* :test #'string=))
                                  (split-words chunk)))
               (ws (ldiff ws (or (member-if #'instruction-p ws)
                                 (member "take" (rest ws) :test #'string=))))
               (item (substitute #\Space #\Bel (unwords ws))))
          (unless (or (zerop (length item)) (find item items :test #'same-item))
            (push item items)))))))

(defn text-items ((text (or null string))) (or null cons)
  "The items in TEXT, nil when there are none."
  (and text (split-items text)))

(defn spoken-list ((items list)) string
  "\"milk\", \"milk and eggs\", \"milk, eggs and bread\"."
  (format nil "~{~a~#[~; and ~:;, ~]~}" items))

(defn list-name ((list (integer 0))) string (first (nth list *lists*)))
(defn list-kind ((list (integer 0))) string (second (nth list *lists*)))

(defn entries ((raw string)) list
  "todo --porcelain list: (id . summary) for each item of its JSON array."
  (let ((items (parse-json raw)))
    (and (vectorp items) (not (stringp items))
         (loop for item across items
               for id = (json-get item "id")
               for summary = (json-get item "summary")
               when (and id (stringp summary))
               collect (cons (princ-to-string id) (trim summary))))))

(defn open-items ((list (integer 0))) list
  (let ((raw (run "todo" "--porcelain" "list" (list-name list)))) (and raw (entries raw))))

(defreplies *duplicates*
  "~a is already on the list. Twice won't make it taste better."
  "Already there: ~a. Your memory, darling.")
(defreplies *added-lots*
  "Added ~a. Are you feeding an army?" "Added ~a. Shopping is cardio, technically."
  "~a. That's a whole trolley, babe.")
(defreplies *added-shopping*
  "~a is on the shopping list." "Added ~a. Buy me something nice too."
  "~a. Got it. Don't eat it all on the way home.")
(defreplies *added-movie* "~a added. Popcorn not included." "Movie night: ~a. Noted.")
(defreplies *added*
  "Added ~a. Now go do it." "Added ~a. Procrastination level: list maker."
  "~a is on your list. Procrastinate responsibly.")

(defn added-speech ((added list) (dupes list) (list (integer 0))) string
  (let ((what (spoken-list added)) (dup (spoken-list dupes)) (kind (list-kind list)))
    (cond ((and (null added) (null dupes)) "Todoman refused. Maybe the list is gone?")
          ((null added) (pick *duplicates* dup))
          (dupes (format nil "Added ~a. ~a was already on there." what dup))
          ((string= kind "shopping")
           (pick (if (>= (length added) 4) *added-lots* *added-shopping*) what))
          ((string= kind "movie") (pick *added-movie* what))
          (t (pick *added* what)))))

(defn add-items ((items list) (list (integer 0))) reply
  (let* ((existing (mapcar #'cdr (open-items list)))
         (dupes (remove-if-not (lambda (i) (find i existing :test #'same-item)) items))
         (added (remove-if-not (lambda (i) (run "todo" "new" "--list" (list-name list) i))
                               (remove-if (lambda (i) (member i dupes :test #'string=)) items))))
    (live (list-name list)
          (format nil "~{+ ~a~^~%~}~:[~;~%~]~{= ~a (already there)~^~%~}" added
                  (and added dupes) dupes)
          (added-speech added dupes list))))

(defreplies *not-there* "~a wasn't on the list anyway." "Can't find ~a on there. Did you dream it?")
(defreplies *crossed-off*
  "Crossed off ~a." "~a, gone. One less thing." "Done with ~a. Proud of you."
  "~a gone. Look at you, adulting.")

(defn done-items ((items list) (list (integer 0))) reply
  (let ((existing (open-items list)) (gone '()) (missing '()))
    (dolist (item items)
      (let ((e (find item existing :key #'cdr :test #'same-item)))
        (if (and e (run "todo" "done" (car e))) (push (cdr e) gone) (push item missing))))
    (setf gone (nreverse gone) missing (nreverse missing))
    (live (list-name list) (format nil "~{✓ ~a~^~%~}" gone)
          (cond ((null gone) (pick *not-there* (spoken-list missing)))
                ((null missing) (pick *crossed-off* (spoken-list gone)))
                (t (format nil "Crossed off ~a. ~a wasn't on there." (spoken-list gone)
                           (spoken-list missing)))))))

(defreplies *cleared*
  "Wiped. ~a things, gone. Fresh start."
  "~a things crossed off in one go. Very decisive. Or lazy."
  "Done. ~a things vanished. I won't tell anyone."
  "All ~a gone. Declaring bankruptcy on your chores, I see.")

(defn clear-list ((list (integer 0)) (confirmed boolean)) reply
  (let ((items (open-items list)) (name (list-name list)))
    (cond ((null items) (live name "" "It's already empty. Nothing to wipe."))
          ((not confirmed)
           (reply name (format nil "~d items" (length items))
                  (format nil "Check off all ~d things on your ~a list?" (length items)
                          (list-kind list))
                  :live t
                  :question (confirm (command-action 'notes-clear list t)
                                     (say-action "Fine, they stay. All of them."))))
          (t (live name "" (pick *cleared* (count-if (lambda (e) (run "todo" "done" (car e)))
                                                     items)))))))

(defreplies *on-list* "Yes, ~a is on there." "~a? Already on the list, sweetie.")
(defreplies *not-on-list*
  "No ~a on the list. Want it? Just say add it." "Nope, no ~a. Living dangerously."
  "No ~a. Panic buying not yet necessary.")

(defn list-contains ((item string) (list (integer 0))) reply
  (live (list-name list) item
        (pick (if (find item (open-items list) :key #'cdr :test #'same-item)
                  *on-list*
                  *not-on-list*)
              item)))

(defreplies *empty-list*
  "Nothing on there. Suspiciously productive." "Empty. Go treat yourself."
  "Nothing. Either very organised or in denial.")

(defn show-list ((list (integer 0))) reply
  (let ((items (mapcar #'cdr (open-items list))))
    (live (list-name list) (format nil "~{~a~^~%~}" items)
          (case (length items)
            (0 (pick *empty-list*))
            (1 (format nil "Just one thing: ~a." (first items)))
            ((2 3) (format nil "~a." (spoken-list items)))
            (otherwise
             (format nil "~d things. Starting with ~a, ~a and ~a. The rest is on screen."
                     (length items) (first items) (second items) (third items)))))))

(defreplies *noted*
  "Noted." "Written down. I won't tell anyone." "Got it. It's in your inbox file."
  "Noted. I'll pretend I didn't read it.")
(defreplies *not-noted*
  "I couldn't write that down. My pen broke."
  "Writing failed. The note was probably too spicy.")

(defn note ((what string)) reply
  (let ((stamp (trim (or (run "date" "+%Y-%m-%d %H:%M") ""))))
    (if (append-line (merge-pathnames "notes/inbox.md" (user-homedir-pathname))
                     (format nil "- ~a ~a" stamp what))
        (live "Note" what (pick *noted*))
        (live "Note" "" (pick *not-noted*)))))

(defn refine-items ((previous action) (ws words)) (or null action)
  "\"and eggs\" after adding milk."
  (let ((items (split-items (unwords ws))))
    (and items (command-action (command-name (action-command previous)) items
                               (second (action-fields previous))))))

(defcommand notes-add
  (:join "![i need to|i want to|i want you to|i'd like you to] (add|put|write|include) {items}"
         "(to|on|onto) [the|my|our] {list} list")
  (:join "![i need to|i want to|i want you to|i'd like you to] (add|put|write|include) {items}"
         "(to|on|onto) [the|my|our] list of {list}")
  "!(add|put|write|include) {items} (to|on|onto) [the|my|our] {list}"
  "!(add|put|write|include) {items} (to|on|onto) [the|my|our] list"
  :kind "Add"
  :named t
  :slots ((items (coalton:List coalton:String)) (list coalton:UFix))
  :parse (let ((items (text-items (slot "items")))) (and items (list items (slot "list" 0))))
  :refine #'refine-items
  :examples ("add milk to the shopping list" "put call mom on my todo list")
  (add-items items list))

(defcommand notes-buy
  (:join "!(we are|we're|i'm|i am) (out of|running out of|low on|almost out of) {items} (so|and)"
         "[will|can|could|would] [you] [please] (add|put|write) (that|it|them|those) (to|on|onto)"
         "[the|my|our] [shopping|grocery] list")
  (:join "!(we are|we're|i'm|i am) (out of|running out of|low on|almost out of) {items}"
         "(will|can|could|would) you [please] (add|put|write) (that|it|them|those) (to|on|onto)"
         "[the|my|our] [shopping|grocery] list")
  (:join "!(we|i) (ran|run) out of {items} (so|and) [will|can|could|would] [you] [please]"
         "(add|put|write) (that|it|them|those) (to|on|onto) [the|my|our] [shopping|grocery] list")
  (:join "!(we|i) (ran|run) out of {items} (will|can|could|would) you [please] (add|put|write)"
         "(that|it|them|those) (to|on|onto) [the|my|our] [shopping|grocery] list")
  "!(i|we) (need|should|have|must) [to] (buy|get) {items}"
  "!we need {items}"
  "!(we are|we're|i'm|i am) (out of|running out of|low on|almost out of) {items}"
  "!(we|i) (ran|run) out of {items}"
  "!(remember|don't forget|do not forget) to (buy|get|pick up) {items}"
  "!(pick up|get|grab|buy) {items} (from|at) the (store|shop|supermarket)"
  "!(buy|pick up) {items}"
  :kind "Add"
  :named t
  :slots ((items (coalton:List coalton:String)) (list coalton:UFix))
  :parse (let ((items (text-items (slot "items")))) (and items (list items 1)))
  :refine #'refine-items
  :examples ("we need milk" "we're out of eggs")
  (add-items items list))

(defcommand notes-clear
  (:join "!(clear|empty|wipe|erase|nuke|blank) [out] [all items on|all the items on|everything"
         "on|everything from|all of] [the|my|our] [whole] {list} list")
  (:join "!(delete|remove|erase|take off|clear) (everything|all items|all the items)"
         "(on|from|in|off) [the|my|our] [whole] {list} list")
  "!make [the|my|our] {list} list (blank|empty)"
  :kind "Clear"
  :named t
  :slots ((list coalton:UFix) (confirmed coalton:Boolean))
  :parse (list (slot "list" 0) nil)
  :examples ("clear my shopping list")
  (clear-list list confirmed))

(defcommand notes-done
  "!(take|cross|scratch|strike) (off|out|away) [the] {items} (from|off) [the|my|our] {list} list"
  "!(take|cross|scratch|strike) (off|out|away) [the] {items} (from|off) [the|my|our] list"
  "!(remove|delete|take|cross|scratch) {items} (from|off) [the|my|our] {list} list"
  "!(remove|delete|take|cross|scratch) {items} (from|off) [the|my|our] list of {list}"
  "!(remove|delete|take|cross|scratch) {items} (from|off) [the|my|our] {list}"
  "!(remove|delete|take|cross|scratch) {items} (from|off) [the|my|our] list"
  "!(tick|check|cross) off {items}"
  :kind "Done"
  :named t
  :slots ((items (coalton:List coalton:String)) (list coalton:UFix))
  :parse (let ((items (text-items (slot "items")))) (and items (list items (slot "list" 1))))
  :refine #'refine-items
  :examples ("remove milk from the shopping list" "tick off eggs")
  (done-items items list))

(defcommand notes-bought
  (:join "!(i|we) (no longer|don't|do not) need {items} (on|in) [the|my|our] {list} list"
         "[anymore|any more]")
  "!(i|we) (no longer|don't|do not) need {items} [anymore|any more]"
  "!(i|we) (bought|got|picked up) {items} [already]"
  "!(i|we) already (bought|got) {items}"
  :kind "Done"
  :named t
  :slots ((items (coalton:List coalton:String)) (list coalton:UFix))
  :parse (let ((items (text-items (slot "items")))) (and items (list items (slot "list" 1))))
  :refine #'refine-items
  :examples ("we bought milk" "i don't need eggs anymore")
  (done-items items list))

(defcommand notes-contains
  "!(is|are) {items} (on|in) [the|my|our] {list} list"
  "!(is|are) {items} (on|in) [the|my|our] {list}"
  "!(did|have) (i|we|you) (add|added|put) {items} [on|to] [the|my|our] {list} [list]"
  "!do (i|we) (need|have to buy|still need) {items}"
  :kind "Contains"
  :named t
  :slots ((item coalton:String) (list coalton:UFix))
  ;; "do i need to get shots" or "a visa for japan" are no shopping.
  :parse (let* ((item (slot "items")) (ws (and item (split-words item))))
           (and ws (string/= (first ws) "to") (<= (length ws) 4) (list item (slot "list" 1))))
  :examples ("is milk on the shopping list" "do we need eggs")
  (list-contains item list))

(defcommand notes-what-to-buy
  "!what (do|should) (i|we) (buy|get) [from|at] [the] [store|shop|supermarket]"
  "!what (do|should) (i|we) need [to buy|to get] [from|at] [the] [store|shop|supermarket]"
  :kind "Show"
  :slots ((list coalton:UFix))
  :parse (list 1)
  :examples ("what do we need from the store")
  (show-list list))

(defcommand notes-show
  "!(what's|what is) on [the|my|our] {list} list"
  "!(read|show|tell) [me] [the|my|our] {list} list"
  (:join "!what (do i have to do|are my todos|are my tasks|is left to do|do i still have to do|do"
         "i need to do) [today|for today]")
  "![tell me] (what's|what is) on [the|my|our] list of {list}"
  "!(read|show|tell) [me] [the|my|our] list of {list}"
  "!(i need|i want) to hear [the|my|our] {list} list"
  :kind "Show"
  :slots ((list coalton:UFix))
  :parse (list (slot "list" 0))
  :examples ("what's on my todo list" "read me the shopping list")
  (show-list list))

(defcommand notes-note
  "!(take|make) a note [that] {items}"
  "!note [that|down] {items}"
  "!write down [that] {items}"
  :kind "Note"
  :slots ((what coalton:String))
  :parse (list (slot "items"))
  :examples ("take a note that the wifi password changed")
  (note what))

(defskill notes)
