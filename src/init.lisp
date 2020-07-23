(defpackage cellar-door.db
  (:use :cl :sqlite)
  (:export #:init
           #:sql-insert
           #:sql-query))
(in-package :cellar-door.db)

(defun sql-query (db query)
  (handler-case (execute-to-list db query)
    (error () "Your memory fails you, the spell isn't quite right...")))

(defun sql-insert (db query)
  (let* ((data   (str:split "values" (string-downcase query)))
         (suffix (str:trim (second data)))
         (query  (format nil "~A values ~A" (str:trim (first data)) (cellar-door.utils:build-escaped-args suffix))))
    (handler-case (apply #'execute-non-query (append `(,db ,query) (cellar-door.utils:build-escaped-values suffix)))
      (error () "Your memory fails you, the spell isn't quite right..."))))

(defun init-enemies (db)
  (execute-non-query db "CREATE TABLE enemy (id INTEGER NOT NULL, name TEXT NOT NULL, PRIMARY KEY(id AUTOINCREMENT))"))

(defun init-combat (db)
  (execute-non-query db "CREATE TABLE combat (id INTEGER NOT NULL, enemy INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT), FOREIGN KEY(enemy) REFERENCES enemies(id))"))

(defun init-elements (db)
  (execute-non-query db "CREATE TABLE elements (id INTEGER NOT NULL, name INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT))")
  (execute-non-query db "INSERT INTO elements (name) VALUES (?)" "fire")
  (execute-non-query db "INSERT INTO elements (name) VALUES (?)" "water")
  (execute-non-query db "INSERT INTO elements (name) VALUES (?)" "ice"))

(defun init-player (db player-name)
  (execute-non-query db "CREATE TABLE player (name TEXT NOT NULL, hp INTEGER NOT NULL, hp_lvl INTEGER NOT NULL, atk INTEGER NOT NULL, atk_lvl INTEGER NOT NULL, def INTEGER NOT NULL, def_lvl INTEGER NOT NULL)")
  (execute-non-query db "INSERT INTO player (name, hp, hp_lvl, atk, atk_lvl, def, def_lvl) VALUES (?, ?, ?, ?, ?, ?, ?)" player-name 100 1 10 1 10 1))

(defun init-spells (db)
  (execute-non-query db "CREATE TABLE spells (id INTEGER NOT NULL, name TEXT NOT NULL, description TEXT NOT NULL, dmg INTEGER NOT NULL, element INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT), FOREIGN KEY(element) REFERENCES elements(id))")
  (execute-non-query db "INSERT INTO spells (name, description, dmg, element) VALUES (?, ?, ?, ?)" "Splash" "You conjour about a flasks worth of water and toss it at a target" 10 2)
  (execute-non-query db "INSERT INTO spells (name, description, dmg, element) VALUES (?, ?, ?, ?)" "Ice Dagger" "You conjour a razor sharp shared of ice and throw it at a target" 10 3)
  (execute-non-query db "INSERT INTO spells (name, description, dmg, element) VALUES (?, ?, ?, ?)" "Fireball" "You conjour a ball of flame and aim it at a target" 10 1))

(defun init-upgrades (db)
  (execute-non-query db "CREATE TABLE upgrade_types (id INTEGER NOT NULL, name INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT))")
  (execute-non-query db "CREATE TABLE upgrade_scaling (id INTEGER NOT NULL, lower INTEGER NOT NULL, upper INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT))")
  (execute-non-query db "CREATE TABLE upgrades (id INTEGER NOT NULL, type INTEGER NOT NULL, lvl INTEGER NOT NULL, scaling INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT), FOREIGN KEY(type) REFERENCES upgrade_types(id))")

  (execute-non-query db "INSERT INTO upgrade_types (name) VALUES (?)" "hp")
  (execute-non-query db "INSERT INTO upgrade_types (name) VALUES (?)" "atk")
  (execute-non-query db "INSERT INTO upgrade_types (name) VALUES (?)" "def")

  (dolist (range '((1 1)        (1.5 1.7)    (1.98 2.4)   (2.43 3.1)   (2.86 3.8)
                   (3.27 4.5)   (3.66 5.2)   (4.02 5.9)   (4.36 6.6)   (4.68 7.3)
                   (4.98 8)     (5.25 8.7)   (5.5 9.4)    (5.73 10.09) (5.93 10.79)
                   (6.11 11.49) (6.27 12.19) (6.41 12.89) (6.52 13.59) (6.61 14.29)
                   (6.68 14.99) (6.73 15.69) (6.75 16.38) (6.77 17.08) (6.79 17.78)
                   (6.81 18.48) (6.83 19.18) (6.84 19.88) (6.85 20.57) (6.86 21.27)))
    (execute-non-query db  "INSERT INTO upgrade_scaling (lower, upper) VALUES (?, ?)" (first range) (second range))))

(defun init (db player-name)
  (init-enemies db)
  (init-upgrades db)
  (init-combat db)
  (init-elements db)
  (init-player db player-name)
  (init-spells db))
