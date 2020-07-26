(defpackage cellar-door.upgrades
  (:use :cl)
  (:export #:main))
(in-package :cellar-door.upgrades)

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
         (stat_lvl (1+     (sqlite:execute-single db (format nil "SELECT ~A_lvl FROM player" stat))))
         (lvl      (first  (sqlite:execute-to-list db "SELECT lower, upper FROM upgrade_scaling WHERE id = ?" stat_lvl)))
         (upper    (first  lvl))
         (lower    (second lvl)))
    (format nil "You found a level ~A ~A upgrade worth ~$%" stat_lvl stat (+ lower (random upper)))))
