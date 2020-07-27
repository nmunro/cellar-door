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

(defun init (db player-name)
  (init-enemies db)
  (cellar-door.upgrades:init db)
  (init-combat db)
  (cellar-door.elements:init db)
  (cellar-door.player:init db player-name)
  (cellar-door.spells:init db))
