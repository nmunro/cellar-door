(defpackage cellar-door.utils
  (:use :cl :sqlite)
  (:export #:find-boundries
           #:build-escaped-args
           #:build-escaped-values))
(in-package :cellar-door.utils)

(defun find-boundries (data &key (match #\,) (order-by #'>))
  (let ((counter 0) (quote-counter 0) (pos '()))
    (dolist (x (coerce data 'list))
      (cond
        ((eq x #\")
         (incf quote-counter))

        ((and (evenp quote-counter) (eq x match))
         (push counter pos)))
      (incf counter))
    (sort pos order-by)))

(defun build-escaped-args (s)
  (format nil "(~{~A~^, ~})" (make-list (1+ (length (find-boundries s))) :initial-element '?)))

(defun build-escaped-values (s)
  (split-str-at-points s))

(defun replace-str-at-points (s points &key (replacement " "))
  (let ((tmp (subseq s 0)))
    (dolist (point points)
      (setf (subseq tmp point (1+ point)) replacement))
    tmp))

(defun split-str-at-points (s)
  (let ((s1 (replace-str-at-points s (find-boundries s))))
    (with-input-from-string (s2 s1)
      (read s2))))

; Tests
(build-escaped-args "(1, \"this is, a test\", 3)")
(build-escaped-values "(1, \"this is, a test\", 3)")
