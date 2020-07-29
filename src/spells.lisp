(defpackage cellar-door.spells
  (:use :cl :sqlite)
  (:export #:main
           #:init))
(in-package :cellar-door.spells)

(defparameter adjectives '("Awful"
                           "Better"
                           "Bloody"
                           "Bright"
                           "Cruel"
                           "Dangerous"
                           "Dark"
                           "Defiant"
                           "Elegant"
                           "Evil"
                           "Fancy"
                           "Fierce"
                           "Fine"
                           "Frantic"
                           "Glorious"
                           "Horrible"
                           "Impossible"
                           "Itchy"
                           "Light"
                           "Magnificent"
                           "Mysterious"
                           "Nasty"
                           "Odd"
                           "Outstanding"
                           "Perfect"
                           "Powerful"
                           "Putrid"
                           "Repulsive"
                           "Scary"
                           "Selfish"
                           "Super"
                           "Tame"
                           "Tender"
                           "Terrible"
                           "Thoughtless"
                           "Unusual"
                           "Vast"
                           "Wandering"
                           "Weary"
                           "Wicked"
                           "Wild"
                           "Wrong"))

(defparameter nouns '("Candle"
                      "Death"
                      "Disease"
                      "Dream"
                      "Energy"
                      "Eye"
                      "Flame"
                      "Ice"
                      "Iron"
                      "Knife"
                      "Lamp"
                      "Machine"
                      "Nail"
                      "Night"
                      "Ocean"
                      "Ghost"
                      "Potato"
                      "Quill"
                      "Rain"
                      "Rainbow"
                      "River"
                      "Spoon"
                      "Wall"
                      "Wire"
                      "Gas"
                      "Glass"))

(defun random-adjective (db)
  (execute-single db "SELECT name FROM spell_adjectives ORDER BY RANDOM() LIMIT 1"))

(defun random-noun (db)
  (execute-single db "SELECT name FROM spell_nouns ORDER BY RANDOM() LIMIT 1"))

(defun random-name (db)
  (format nil "~A ~A" (random-adjective db) (random-noun db)))

(defun populate (db tbl items)
  (dolist (item items)
    (execute-non-query db (format nil "INSERT INTO ~A (name) VALUES (?)" tbl) item)))

(defun init (db)
  (execute-non-query db "CREATE TABLE spell_adjectives (id INTEGER NOT NULL, name TEXT NOT NULL, PRIMARY KEY(id AUTOINCREMENT))")
  (execute-non-query db "CREATE TABLE spell_nouns (id INTEGER NOT NULL, name TEXT NOT NULL, PRIMARY KEY(id AUTOINCREMENT))")

  (populate db "spell_adjectives" adjectives)
  (populate db "spell_nouns" nouns)

  (execute-non-query db "CREATE TABLE spells (id INTEGER NOT NULL, name TEXT NOT NULL, dmg INTEGER NOT NULL, element INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT), FOREIGN KEY(element) REFERENCES elements(id))")
  (execute-non-query db "INSERT INTO spells (name, dmg, element) VALUES (?, ?, ?)" (random-name db) 10 (cellar-door.elements:random-element db))
  (execute-non-query db "INSERT INTO spells (name, dmg, element) VALUES (?, ?, ?)" (random-name db) 10 (cellar-door.elements:random-element db)))

(defun main (db)
  "Randomly generate a spell"
  (let ((name    (random-name db))
        (element (cellar-door.elements:random-element db)))
    (format nil "You found \"~A\"; a level ~A, ~A spell" name 1 element)))
