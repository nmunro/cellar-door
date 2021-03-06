(defpackage cellar-door
  (:use :cl)
  (:export #:main))
(in-package :cellar-door)

(defun game-over-p (db)
  (>= 0 (sqlite:execute-single db "SELECT hp FROM player")))

(defun main ()
  (setf *random-state* (make-random-state t))
  (sqlite:with-open-database (db ":memory:")
    (format t "Please enter your name: ")
    (force-output)

    (let ((player-name (read-line)))
      (cellar-door.db:init db player-name)
      (cellar-door.intro:main player-name))

    (cellar-door.world:new-world)
    (format t "~A~%" (cellar-door.upgrades:main db))

    (do () (nil)
      (when (game-over-p db)
        (return-from main (format t "~c[31mYou died!~c[0m" #\ESC #\ESC)))

      (format t "::> ")
      (force-output)

      (let ((query (str:trim (read-line))))
        (cond
          ((string= (string-downcase query) "help")
           (cellar-door.help:main))

          ((string= (string-downcase query) "quit")
           (let ((answer (yes-or-no-p "Are you sure you want to quit?")))
             (when answer
               (return-from main (format t "Oh, ok, bye!~%")))))

          ((string= (string-downcase query) "map")
           (cellar-door.world:draw-known-city)
           (format t "You sketch some more information into your map...~%"))

          ((str:starts-with-p "walk" query :ignore-case t)
           (let ((place (first (str:split "walk " query :omit-nulls t))))
             (format t "You attempt to walk into room ~A.~%" place)

             (let ((tmp (cellar-door.world:walk (parse-integer place :junk-allowed t))))
               (cond
                 ((eq tmp :spell)
                  (format t "~A!~%" (cellar-door.spells:main db)))

                 ((eq tmp :upgrade)
                  (format t "~A!~%" (cellar-door.upgrades:main db)))

                 ((eq tmp :monster)
                  (format t "You encountered a monster!~%"))

                 (t (format t "The room is empty!~%"))))))

          ((str:starts-with-p "SELECT" query :ignore-case t)
           (let ((results (cellar-door.db:sql-query db query)))
             (if (listp results)
                (dolist (result results)
                  (format t "~A~%" result))
                (format t "~A~%" results))))

          ((str:starts-with-p "UPDATE" query :ignore-case t)
            (cellar-door.db:sql-query db query))

          ((str:starts-with-p "INSERT" query :ignore-case t)
            (cellar-door.db:sql-insert db query))

          ((str:starts-with-p "DELETE" query :ignore-case t)
            (cellar-door.db:sql-query db query))

          (t (format t "Unrecognised~%")))))))
