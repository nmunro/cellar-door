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

(defun init (db)
  (execute-non-query db "CREATE TABLE spell_adjectives (id INTEGER NOT NULL, name TEXT NOT NULL, PRIMARY KEY(id AUTOINCREMENT))")
  (dolist (adjective adjectives)
    (execute-non-query db "INSERT INTO spell_adjectives (name) VALUES (?)" adjective))

  (execute-non-query db "CREATE TABLE spell_nouns (id INTEGER NOT NULL, name TEXT NOT NULL, PRIMARY KEY(id AUTOINCREMENT))")
  (dolist (noun nouns)
    (execute-non-query db "INSERT INTO spell_nouns (name) VALUES (?)" noun))

  (execute-non-query db "CREATE TABLE spells (id INTEGER NOT NULL, name TEXT NOT NULL, description TEXT NOT NULL, dmg INTEGER NOT NULL, element INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT), FOREIGN KEY(element) REFERENCES elements(id))")
  (execute-non-query db "INSERT INTO spells (name, description, dmg, element) VALUES (?, ?, ?, ?)" "Splash" "You conjour about a flasks worth of water and toss it at a target" 10 2)
  (execute-non-query db "INSERT INTO spells (name, description, dmg, element) VALUES (?, ?, ?, ?)" "Ice Dagger" "You conjour a razor sharp shared of ice and throw it at a target" 10 3)
  (execute-non-query db "INSERT INTO spells (name, description, dmg, element) VALUES (?, ?, ?, ?)" "Fireball" "You conjour a ball of flame and aim it at a target" 10 1))

(defun generate-name (db)
  (format nil "~A ~A" (random-adjective db) (random-noun db)))

(defun main (db)
  "Randomly generate a spell"
  (let ((name (generate-name db))
        (element (cellar-door.elements:random-element db)))
    (format nil "You found \"~A\"; a level ~A, ~A spell" name 1 element)))
