(defpackage cellar-door.elements
  (:use :cl :sqlite)
  (:export #:main
           #:random-element
           #:init))
(in-package :cellar-door.elements)

(defun random-element (db)
  (execute-single db "SELECT name FROM elements ORDER BY RANDOM() LIMIT 1"))

(defun init (db)
  (execute-non-query db "CREATE TABLE elements (id INTEGER NOT NULL, name INTEGER NOT NULL, strong INTEGER NOT NULL, weak INTEGER NOT NULL, PRIMARY KEY(id AUTOINCREMENT))")
  (execute-non-query db "INSERT INTO elements (name, strong, weak) VALUES (?, ?, ?)" "white" 3 4)
  (execute-non-query db "INSERT INTO elements (name, strong, weak) VALUES (?, ?, ?)" "blue" 4 5)
  (execute-non-query db "INSERT INTO elements (name, strong, weak) VALUES (?, ?, ?)" "black" 5 1)
  (execute-non-query db "INSERT INTO elements (name, strong, weak) VALUES (?, ?, ?)" "red" 1 2)
  (execute-non-query db "INSERT INTO elements (name, strong, weak) VALUES (?, ?, ?)" "green" 2 3))
