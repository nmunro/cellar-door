(defpackage cn-db/tests/main
  (:use :cl
        :cn-db
        :rove))
(in-package :cn-db/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :cn-db)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
