;;;; macro utilities as shown on page 170 of ANSI Common Lisp

;;;; FOR macro
(defmacro for (var start stop &body body)
  (let ((gstop (gensym)))
    `(do ((,var ,start (1+ ,var))
	  (,gstop ,stop))
	 ((> ,var ,gstop))
       ,@body)))

;;;; IN macro
(defmacro in (obj &rest choices)
  (let ((insym (gensym)))
    `(let ((,insym ,obj))
       (or ,@(mapcar #'(lambda (c) `(eql ,insym ,c))
		     choices)))))

;;;; RANDOM-CHOICES macro
(defmacro random-choices (&rest exprs)
  `(case (random ,(length exprs))
     ,@(let ((key -1))
	    (mapcar #'(lambda (expr)
			`(,(incf key) ,expr))
		    exprs))))

;;;; AVG macro
(defmacro avg (&rest args)
  `(/ (+ ,@args) ,(length args)))

;;;; WITH_GENSYMS macro
(defmacro with-gensyms (syms &body body)
  `(let ,(mapcar #'(lambda (s)
		     `(,s (gensym)))
		 syms)
     ,@body))

;;;; AIF macro
(defmacro aif (test then &optional else)
  `(let ((it ,test))
     (if it ,then ,else)))
