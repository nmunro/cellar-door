(defpackage cellar-door.elements
  (:use :cl :sqlite)
  (:export #:main
           #:init))
(in-package :cellar-door.elements)

(defun init (db)
  (execute-non-query db "CREATE TABLE elements (id INTEGER NOT NULL, name INTEGER NOT NULL, strong INTEGER NOT NULL, weak INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT))")
  (execute-non-query db "INSERT INTO elements (name, strong, weak) VALUES (?, ?, ?)" "light" 3 4)
  (execute-non-query db "INSERT INTO elements (name, strong, weak) VALUES (?, ?, ?)" "water" 4 5)
  (execute-non-query db "INSERT INTO elements (name, strong, weak) VALUES (?, ?, ?)" "dark" 5 1)
  (execute-non-query db "INSERT INTO elements (name, strong, weak) VALUES (?, ?, ?)" "fire" 1 2)
  (execute-non-query db "INSERT INTO elements (name, strong, weak) VALUES (?, ?, ?)" "nature" 2 3))

(defun main (db)
  ; Populate database table
  nil)
