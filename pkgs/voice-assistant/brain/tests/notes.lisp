;;; Ported from notes.rs.

(in-package #:va-tests)

(define-test notes-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "~a"
               '(("Add milk to the shopping list." "Notes(Add { items: [\"milk\"], list: 1 })")
                 ("Put Dune on my movie list" "Notes(Add { items: [\"dune\"], list: 2 })")
                 ("Add fix the CI to my todo list"
                  "Notes(Add { items: [\"fix the ci\"], list: 0 })")
                 ("Add some milk, eggs and a loaf of bread to the shopping list"
                  "Notes(Add { items: [\"milk\", \"eggs\", \"loaf of bread\"], list: 1 })")
                 ("Add salt and pepper to shopping"
                  "Notes(Add { items: [\"salt and pepper\"], list: 1 })")
                 ("Put bananas on the grocery list." "Notes(Add { items: [\"bananas\"], list: 1 })")
                 ("We're out of eggs." "Notes(Add { items: [\"eggs\"], list: 1 })")
                 ("We're running out of coffee" "Notes(Add { items: [\"coffee\"], list: 1 })")
                 ("We ran out of toilet paper." "Notes(Add { items: [\"toilet paper\"], list: 1 })")
                 ("I'm low on milk" "Notes(Add { items: [\"milk\"], list: 1 })")
                 ("I'm out of kleenex so will you put that on my shopping list"
                  "Notes(Add { items: [\"kleenex\"], list: 1 })")
                 ("I'm low on cheerios, can you add that to the shopping list"
                  "Notes(Add { items: [\"cheerios\"], list: 1 })")
                 ("I ran out of apples so add them to my list"
                  "Notes(Add { items: [\"apples\"], list: 1 })")
                 ("Take off fries from the shopping list"
                  "Notes(Done { items: [\"fries\"], list: 1 })")
                 ("Please take away the fries from the shopping list"
                  "Notes(Done { items: [\"fries\"], list: 1 })")
                 ("We no longer need fries" "Notes(Done { items: [\"fries\"], list: 1 })")
                 ("I'm out of apples, add that to my list"
                  "Notes(Add { items: [\"apples\"], list: 1 })")
                 ("I'm out of bananas; add to shopping list"
                  "Notes(Add { items: [\"bananas\"], list: 1 })")
                 ("I am out of trash bags and need them to be added to my shopping list"
                  "Notes(Add { items: [\"trash bags\"], list: 1 })")
                 ("I no longer need milk, so forget that"
                  "Notes(Done { items: [\"milk\"], list: 1 })")
                 ("Cross out bananas from my shopping list"
                  "Notes(Done { items: [\"bananas\"], list: 1 })")
                 ("I need to buy batteries" "Notes(Add { items: [\"batteries\"], list: 1 })")
                 ("Don't forget to buy cat food." "Notes(Add { items: [\"cat food\"], list: 1 })")
                 ("Pick up 2 litres of milk from the store"
                  "Notes(Add { items: [\"2 litres of milk\"], list: 1 })")
                 ("We need more butter and cheese"
                  "Notes(Add { items: [\"butter\", \"cheese\"], list: 1 })")
                 ("Remove milk from the shopping list" "Notes(Done { items: [\"milk\"], list: 1 })")
                 ("Take eggs off the shopping list" "Notes(Done { items: [\"eggs\"], list: 1 })")
                 ("I bought milk and eggs." "Notes(Done { items: [\"milk\", \"eggs\"], list: 1 })")
                 ("Is milk on my shopping list?" "Notes(Contains { item: \"milk\", list: 1 })")
                 ("Do we need eggs?" "Notes(Contains { item: \"eggs\", list: 1 })")
                 ("What's on my shopping list?" "Notes(Show(1))")
                 ("What do I need to buy?" "Notes(Show(1))")
                 ("What do we need from the store?" "Notes(Show(1))")
                 ("What do I have to do?" "Notes(Show(0))")
                 ("Take a note that the wifi password is on the fridge"
                  "Notes(Note(\"the wifi password is on the fridge\"))")
                 ("what do i have to do today" "Notes(Show(0))")
                 ("what is left to do today" "Notes(Show(0))")
                 ("tell me what is on the list of things to do" "Notes(Show(0))")
                 ("i need to hear my todo list" "Notes(Show(0))")
                 ("what are my tasks for today" "Notes(Show(0))")
                 ("i want you to add cleaning the toilet to my to do list"
                  "Notes(Add { items: [\"cleaning the toilet\"], list: 0 })")
                 ("please include feeding the fish on my to do list"
                  "Notes(Add { items: [\"feeding the fish\"], list: 0 })")
                 ("put wash the counters down on my list of pending tasks"
                  "Notes(Add { items: [\"wash the counters down\"], list: 0 })")
                 ("i need to add the chore of vacuuming to my task list"
                  "Notes(Add { items: [\"vacuuming\"], list: 0 })")
                 ("please remove laundry from my list of chores"
                  "Notes(Done { items: [\"laundry\"], list: 0 })")
                 ("please clear out my whole to do list"
                  "Notes(Clear { list: 0, confirmed: false })")
                 ("erase all items on my todo list" "Notes(Clear { list: 0, confirmed: false })")
                 ("delete everything from the task list"
                  "Notes(Clear { list: 0, confirmed: false })")
                 ("make my todo list blank" "Notes(Clear { list: 0, confirmed: false })")
                 ("empty the shopping list" "Notes(Clear { list: 1, confirmed: false })")))
    (parses-not "Notes" '("do i need to get any shots before volunteering in the congo"
                          "do i need a visa for japan if i am staying a week"))))

(define-test notes-follow-ups
  :parent va-tests
  (with-focus (:previous (va::parse-text "add milk to the shopping list"))
    (is equal "Notes(Add { items: [\"eggs\"], list: 1 })" (parsed "and eggs"))))

(define-test notes-items
  :parent va-tests
  (is equal '(("3" . "Milk") ("7" . "Eggs"))
      (va::entries (concatenate 'string "[{\"id\":3,\"summary\":\"Milk\"},"
                                "{\"id\":\"7\",\"summary\":\" Eggs \"},{\"id\":9}]")))
  (false (va::entries "garbage"))
  (is equal '("milk" "eggs" "bread") (va::split-items "milk, eggs, and bread"))
  (is equal '("salt and pepper" "mac and cheese")
      (va::split-items "salt and pepper and mac and cheese"))
  (is equal '("milk") (va::split-items "some milk and more milk"))
  (is equal '("egg") (va::split-items "an egg and eggs"))
  (false (va::split-items "a"))
  (is equal '("take out the trash") (va::split-items "take out the trash"))
  (true (va::same-item "Eggs" "egg"))
  (true (va::same-item " Milk" "milk"))
  (false (va::same-item "milk" "silk"))
  (is equal "a, b and c" (va::spoken-list '("a" "b" "c")))
  (is equal "a and b" (va::spoken-list '("a" "b"))))

(define-test notes-effects
  :parent va-tests
  (let ((effects (va::fake-effects (va::note "buy a plant"))))
    (is equal :append (first (car (last effects))))
    (true (search "- " (third (car (last effects)))))))
