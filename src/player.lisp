(defpackage cellar-door.player
  (:use :cl :sqlite)
  (:export #:init))
(in-package :cellar-door.player)

(defun init (db player-name)
  (execute-non-query db "CREATE TABLE player (name TEXT NOT NULL, hp INTEGER NOT NULL, hp_lvl INTEGER NOT NULL, hp_scaling INTEGER NOT NULL, atk INTEGER NOT NULL, atk_lvl INTEGER NOT NULL, atk_scaling INTEGER NOT NULL, def INTEGER NOT NULL, def_lvl INTEGER NOT NULL, def_scaling INTEGER NOT NULL)")
  (execute-non-query db "INSERT INTO player (name, hp, hp_lvl, hp_scaling, atk, atk_lvl, atk_scaling, def, def_lvl, def_scaling) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?)" player-name 100 1 0 10 1 0 10 1 0))
