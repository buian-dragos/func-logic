(defun postorder (node)
  (if node  ;; left subtree
    (postorder (car (cdr node)))    ;; left child
  )
  (if node  ;; right subtree
    (postorder (car (cdr (cdr node)))) ;; right child
  )
  (if node  ;; recursive print
    (print (car node))))
    
(let ((tree '(A (B) (C (D) (E)))))
  (postorder tree))