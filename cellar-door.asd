(defsystem "cellar-door"
  :version "0.1.0"
  :author ""
  :license ""
  :depends-on (:sqlite :str)
  :components ((:module "src"
                :components
                ((:file "utils")
                 (:file "init")
                 (:file "intro")
                 (:file "upgrades")
                 (:file "world")
                 (:file "help")
                 (:file "main"))))
  :description ""
  :in-order-to ((test-op (test-op "cellar-door/tests"))))

(defsystem "cellar-door/tests"
  :author ""
  :license ""
  :depends-on ("cellar-door"
               "rove")
  :components ((:module "tests"
                :components
                ((:file "main"))))
  :description "Test system for cellar-door"
  :perform (test-op (op c) (symbol-call :rove :run c)))
