;;;; the function builder examples as shown on page 110 of ANSI Common Lisp

;; compose takes one or more functions and returns a new function in which all of them are applied in succession
(defun compose (&rest fns)
  (destructuring-bind (fn1 . rest) (reverse fns)
    #'(lambda (&rest args)
	(reduce #'(lambda (v f) (funcall f v))
			  rest
			  :initial-value (apply fn1 args)))))

;; disjoin returns a predicate that returns true when any of the predicates return true
(defun disjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((disj (apply #'disjoin fns)))
	#'(lambda (&rest args)
	    (or (apply fn args) (apply disj args))))))

;; conjoin returns a predicate that returns true when all of the predicates return true
(defun conjoin (fn &rest fns)
  (if (null fns)
      fn
      (let ((conj (apply #'conjoin fns)))
	#'(lambda (&rest args)
	    (and (apply fn args) (apply conj args))))))

;; curry takes a function and some of the arguments to it, and returns a new function that expects the rest of the arguments
(defun curry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args args2))))

;; rcurry is like curry, but is used when the order of the arguments matter
(defun rcurry (fn &rest args)
  #'(lambda (&rest args2)
      (apply fn (append args2 args))))
