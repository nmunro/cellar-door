(defpackage cellar-door.enemies
  (:use :cl)
  (:export #:random-enemy))
(in-package :cellar-door.enemies)

(defparameter nouns   '("Dark" "Fire" "Evil" "Shadow" "Slimey"))

(defparameter enemies '("Imp" "Angel" "Hound" "Wraith" "Grue"
                        "Beast"))

(defun random-enemy (db)
  (let ((name (format nil "~A ~A" (nth (random (length nouns)) nouns) (nth (random (length enemies)) enemies)))
        (element (cellar-door.elements:random-element db)))
    (format nil "You encounter a level ~A ~A ~A" 1 element name)))
