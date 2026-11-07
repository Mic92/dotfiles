;;; Ported from spell.rs.

(in-package #:va-tests)

(define-test spell-phrasings
  :parent va-tests
  (with-focus ()
    (parses-as "Spell(Word(~s))"
               '(("spell potato" "potato") ("how do you spell tomato" "tomato")
                 ("How is water spelled?" "water") ("water is spelled how" "water")
                 ("can you spell \"diatribe\" for me" "diatribe")
                 ("can you spell out the word special for me" "special")
                 ("what's the right way to spell miscellaneous" "miscellaneous")
                 ("i need to know how to spell excellent" "excellent")
                 ("tell me how to spell anonymous" "anonymous")
                 ("what is the correct spelling of judgment" "judgment")
                 ("give me the spelling of potato" "potato")
                 ("i wish i knew how to spell mississippi" "mississippi")
                 ("can you remind me how to spell because" "because")
                 ("how is the word economy spelled" "economy")
                 ("i don't know how to spell squirrel" "squirrel")
                 ("i'd like to know how acommodate is spelled" "acommodate")
                 ("malignant is spelled how exactly" "malignant")
                 ("i need help spelling superficial" "superficial")
                 ("how do you spell the word comity, i need to know" "comity")
                 ("i need to know the proper spelling of curiosity" "curiosity")))
    (parses-as "Spell(Count { letter: ~a, word: ~s })"
               '(("how many r's are in restaurant" "Some('r')" "restaurant")
                 ("tell me how many p's are in apple" "Some('p')" "apple")
                 ("do you know how many o's are in honor" "Some('o')" "honor")
                 ("the word 'you' got how many letters" "None" "you")
                 ("count the number of the letter a in happiness" "Some('a')" "happiness")))
    (parses-not "Spell" '("spell it" "how do you spell new york"))))

(define-test spell-spoken
  :parent va-tests
  (is equal "P, O, T, A, T, O" (va::letters "potato"))
  (is equal "M, I, double S, I, double S, I, double P, I" (va::letters "mississippi"))
  (is equal "restaurant has 2 R's" (va::letter-count #\r "restaurant"))
  (is equal "There's no Z in apple" (va::letter-count #\z "apple"))
  (is equal "you has 3 letters" (va::letter-count nil "you")))
