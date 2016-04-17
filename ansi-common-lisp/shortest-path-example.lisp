;;;; the shortest path example as shown on page 51 of ANSI common lisp

;;networks
(defvar minimal)
(setf minimal '((a b c) (b c) (c d)))

(defvar another-network)
(setf another-network '((a b c d e) (d s e f g a b) (x s y g a e d)))

(defun shortest-path (start end net)
  (bfs end (list (list start)) net))

(defun bfs (end queue net)
  (if (null queue)
      nil
      (let ((path (car queue)))
	(let ((node (car path)))
	  (if (eql node end)
	      (reverse path)
	      (bfs end
		   (append (cdr queue)
			   (new-paths path node net))
		   net))))))

(defun new-paths (path node net)
  (mapcar #'(lambda (n)
	      (cons n path))
	  (cdr (assoc node net))))

