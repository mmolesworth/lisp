;;;; utility functions as shown on page 105 of ANSI Common Lisp


;; determines if a list has just one element
(defun single? (lst)
  (and (consp lst)
       (null (cdr lst))))

;; appends an object to the end of a list
(defun append1 (obj lst)
  (append lst (list obj)))

;; returns a list that calls a function on the integers from 0 to n-1
(defun map-int (fn n)
  (let ((acc nil))
    (dotimes (i n)
      (push (funcall fn i) acc))
    (nreverse acc)))

;; returns a list of non-nil values
(defun filter (fn lst)
  (let ((acc nil))
    (dolist (x lst)
      (let ((val (funcall fn x)))
	(if val (push val acc))))
    (nreverse acc)))

;;returns the element of a list with the highest score, according to some scoring function
(defun most (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (max (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (> score max)
	      (setf wins obj
		    max score))))
	(values wins max))))

;;returns the element of a list with the lowest score, according to some scoring function
(defun least (fn lst)
  (if (null lst)
      (values nil nil)
      (let* ((wins (car lst))
	     (min (funcall fn wins)))
	(dolist (obj (cdr lst))
	  (let ((score (funcall fn obj)))
	    (when (< score min)
	      (setf wins obj
		    min score))))
	(values wins min))))

;; sum a list of integers
(defun sum (lst)
  (let ((sum 0))
	   (dolist (x lst sum)
	     (setf sum (+ x sum)))))

