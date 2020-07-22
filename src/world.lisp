(defpackage cellar-door.world
  (:use :cl :sqlite)
  (:export #:new-world
           #:draw-known-city
           #:walk))
(in-package :cellar-door.world)

(defparameter *max-label-length* 50)
(defparameter *city-nodes* nil)
(defparameter *city-edges* nil)
(defparameter *player-pos* nil)
(defparameter *visited-nodes* nil)
(defparameter *node-num* 30)
(defparameter *edge-num* 45)

(defun dot-name (exp)
  (substitute-if #\_ (complement #'alphanumericp) (prin1-to-string exp)))

(defun dot-label (exp)
  (if exp
    (if (> (length exp) *max-label-length*)
            (concatenate 'string (subseq exp 0 (- *max-label-length* 3)) "...")
            exp)
    ""))

(defun nodes->dot (nodes)
  (mapc (lambda (node)
          (fresh-line)
          (princ (dot-name (car node)))
          (princ "[label=\"")
          (princ (dot-label node))
          (princ "\"];"))
        nodes))

(defun edges->dot (edges)
  (mapc (lambda (node)
          (mapc (lambda (edge)
                  (fresh-line)
                  (princ (dot-name (car node)))
                  (princ "->")
                  (princ (dot-name (car edge)))
                  (princ "[label=\"")
                  (princ (dot-label (cdr edge)))
                  (princ "\"];"))
                (cdr node)))
        edges))

(defun graph->dot (nodes edges)
  (princ "digraph{")
  (nodes->dot nodes)
  (edges->dot edges)
  (princ "}"))

(defun dot->png (fname thunk)
  (with-open-file (*standard-output* fname :direction :output :if-exists :supersede)
    (funcall thunk))
  (uiop:run-program (concatenate 'string "dot -Tpng -O " fname))
  (uiop:run-program (concatenate 'string "rm " fname)))

(defun graph->png (fname nodes edges)
  (dot->png fname (lambda () (graph->dot nodes edges))))

(defun uedges->dot (edges)
  (maplist (lambda (lst)
             (mapc (lambda (edge)
                     (unless (assoc (car edge) (cdr lst))
                       (fresh-line)
                       (princ (dot-name (caar lst)))
                       (princ "--")
                       (princ (dot-name (car edge)))
                       (princ "[label=\"")
                       (princ (dot-label (cdr edge)))
                       (princ "\"];")))
                   (cdar lst)))
           edges))

(defun ugraph->dot (nodes edges)
  (princ "graph{")
  (nodes->dot nodes)
  (uedges->dot edges)
  (princ "}"))

(defun ugraph->png (fname nodes edges)
  (dot->png fname (lambda () (ugraph->dot nodes edges))))

(defun random-node ()
  (1+ (random *node-num*)))

(defun edge-pair (a b)
  (unless (eql a b)
    (list (cons a b) (cons b a))))

(defun make-edge-list ()
  (apply #'append (loop repeat *edge-num* collect (edge-pair (random-node) (random-node)))))

(defun direct-edges (node edge-list)
  (remove-if-not (lambda (x) (eql (car x) node)) edge-list))

(defun get-connected (node edge-list)
  (let ((visited nil))
    (labels
        ((traverse (node)
           (unless (member node visited)
             (push node visited)
             (mapc (lambda (edge) (traverse (cdr edge)))
                   (direct-edges node edge-list)))))
      (traverse node))
    visited))

(defun find-islands (nodes edge-list)
  (let ((islands nil))
    (labels ((find-island (nodes)
               (let* ((connected (get-connected (car nodes) edge-list))
                      (unconnected (set-difference nodes connected)))
                 (push connected islands)
                 (when unconnected
                   (find-island unconnected)))))
      (find-island nodes))
    islands))

(defun connect-with-bridges (islands)
  (when (cdr islands)
    (append (edge-pair (caar islands) (caar islands))
            (connect-with-bridges (cdr islands)))))

(defun connect-all-islands (nodes edge-list)
  (append (connect-with-bridges (find-islands nodes edge-list)) edge-list))

(defun make-city-edges ()
  (let* ((nodes (loop for i from 1 to *node-num* collect i))
         (edge-list (connect-all-islands nodes (make-edge-list))))
    (edges-to-alist edge-list)))

(defun edges-to-alist (edge-list)
  (mapcar (lambda (node1)
            (cons node1
                  (mapcar (lambda (edge) (list (cdr edge)))
                          (remove-duplicates (direct-edges node1 edge-list) :test #'equal))))
          (remove-duplicates (mapcar #'car edge-list))))

(defun make-city-nodes ()
  ; Random encounters need to be created here
  (loop for n from 1 to *node-num*
          collect (append (list n)
                          (cond
                            ((= (random 4) 0)
                             '(upgrade))

                            ((= (random 4) 1)
                             '(monster))

                            ((= (random 4) 2)
                             '(spell))))))

(defun find-empty-node ()
  (let ((x (random-node)))
    (if (cdr (assoc x *city-nodes*))
        (find-empty-node)
        x)))

(defun draw-known-city ()
  (ugraph->png "known-city" (known-city-nodes) (known-city-edges)))

(defun known-city-nodes ()
  (mapcar (lambda (node)
            (if (member node *visited-nodes*)
                (let ((n (assoc node *city-nodes*)))
                  (if (eql node *player-pos*)
                      (append n '(*))
                      n))
                (list node)))
          (remove-duplicates
           (append *visited-nodes*
                   (mapcan (lambda (node)
                             (mapcar #'car
                                     (cdr (assoc node *city-edges*))))
                           *visited-nodes*)))))

(defun known-city-edges ()
  (mapcar (lambda (node)
            (cons node (mapcar (lambda (x)
                                 (if (member (car x) *visited-nodes*)
                                     x
                                     (list (car x))))
                               (cdr (assoc node *city-edges*)))))
          *visited-nodes*))

(defun new-world ()
  (setf *city-edges* (make-city-edges))
  (setf *city-nodes* (make-city-nodes))
  (setf *player-pos* (find-empty-node))
  (setf *visited-nodes* (list *player-pos*)))

(defun walk (pos)
  (let ((edge (assoc pos (cdr (assoc *player-pos* *city-edges*)))))
    (if edge
        (handle-new-place pos)
        (format t "That location does not exist!~%"))))

(defun handle-new-place (pos)
  (let ((node (assoc pos *city-nodes*)))
    (pushnew pos *visited-nodes*)
    (setf *player-pos* pos)
    (draw-known-city)

    ; Check random encounters here
    (cond
      ((member 'spell node)
       (format t "You found a new spell!~%"))

      ((member 'upgrade node)
       (format t "You found an upgrade!~%"))

      ((member 'monster node)
       (format t "You encountered a monster!~%"))

      (t (format t "The building is empty.~%")))))
