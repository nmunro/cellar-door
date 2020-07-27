(defpackage cellar-door.upgrades
  (:use :cl :sqlite)
  (:export #:main
           #:init))
(in-package :cellar-door.upgrades)

(defun init (db)
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

(defun pick-stat ()
  (let ((rnd (random 3)))
    (cond
     ((= 0 rnd)
       'hp)

      ((= 1 rnd)
       'atk)

      ((= 2 rnd)
       'def))))

(defun main (db)
  "Randomly generate an upgrade"
  ;; Get the player hp, atk or def level and generate a random new level
  (let* ((stat     (pick-stat))
         (stat_lvl (1+     (execute-single db (format nil "SELECT ~A_lvl FROM player" stat))))
         (lvl      (first  (execute-to-list db "SELECT lower, upper FROM upgrade_scaling WHERE id = ?" stat_lvl)))
         (upper    (first  lvl))
         (lower    (second lvl)))
    (format nil "You found a level ~A ~A upgrade worth ~$%" stat_lvl stat (+ lower (random upper)))))
