(defpackage cellar-door.upgrades
  (:use :cl)
  (:export #:main))
(in-package :cellar-door.upgrades)

(defun main (db)
  (generate-upgrade db))

(defun generate-upgrade (db)
  (let* ((rnd (random 3))
         (lvl (first (sqlite:execute-to-list db "SELECT lower, upper FROM upgrade_scaling WHERE id = ?" 2)))
         (upper (first lvl))
         (lower (second lvl)))
    (cond
      ((= rnd 0)
       (format nil "HP upgrade: ~A"  (+ lower (random upper))))

      ((= 1 rnd)
       (format nil "ATK upgrade: ~A" (+ lower (random upper))))

      ((= 2 rnd)
       (format nil "DEF upgrade: ~A" (+ lower (random upper)))))))
