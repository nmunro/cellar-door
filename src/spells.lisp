(defpackage cellar-door.spells
  (:use :cl :sqlite)
  (:export #:main
           #:init))
(in-package :cellar-door.spells)

(defun init (db)
  (execute-non-query db "CREATE TABLE spells (id INTEGER NOT NULL, name TEXT NOT NULL, description TEXT NOT NULL, dmg INTEGER NOT NULL, element INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT), FOREIGN KEY(element) REFERENCES elements(id))")
  (execute-non-query db "INSERT INTO spells (name, description, dmg, element) VALUES (?, ?, ?, ?)" "Splash" "You conjour about a flasks worth of water and toss it at a target" 10 2)
  (execute-non-query db "INSERT INTO spells (name, description, dmg, element) VALUES (?, ?, ?, ?)" "Ice Dagger" "You conjour a razor sharp shared of ice and throw it at a target" 10 3)
  (execute-non-query db "INSERT INTO spells (name, description, dmg, element) VALUES (?, ?, ?, ?)" "Fireball" "You conjour a ball of flame and aim it at a target" 10 1))

(defun main (db)
  "Randomly generate a spell"
  (format nil "You found a level ~A, ~A spell" 1 (cellar-door.elements:random-element db)))
