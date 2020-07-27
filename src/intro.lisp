(defpackage cellar-door.intro
  (:use :cl)
  (:export #:main))
(in-package :cellar-door.intro)

(defun main (player-name)
  (format t "Welcome ~c[32m~A~c[0m, you are a techmage, one with the ability to acquire seemingly infinite knowledge, and manipulate the world built of information.~%" #\ESC player-name #\ESC))
