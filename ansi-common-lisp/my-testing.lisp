(defun my-member (element lst)
  (if (null lst)
      nil
      (if (eql element (car lst))
	  lst
	  (my-member element (cdr lst)))))

(defun my-copy-tree (tree)
  (if (atom tree)
      tree
      (cons (car tree)
	   (copy-tree (cdr tree)))))
