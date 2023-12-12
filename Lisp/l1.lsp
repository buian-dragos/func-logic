; a

; nElem(n,l1l2..lm) = | nil, n < 1 or m == 0
;                     | l1, n == 1 
;                     | nElem(n-1, l2l3..lm), otherwise

(defun nElem (n lst)
  (if (or (< n 1) (null lst))
      nil
      (if (= n 1)
          (car lst)
          (nElem (- n 1) (cdr lst)))))

(let ((listA '(0 1 2 3 4 5))
      (n 3))
  (format t "a: ~a~%" (nElem n listA)))

; b

; checkMember(e, l1l2..ln) = | nil, n == 0
;                            | true, e == l1
;                            | checkMember(e, l1) or checkMember(l2l3..ln), if l1 is a list
;                            | checkMember(e, l2l3..ln), otherwise

(defun checkMember (e lst)
  (if (null lst)
      nil
      (if (equal e (car lst))
          t
          (if (listp (car lst))
              (or (checkMember e (car lst))
                  (checkMember e (cdr lst)))
              (checkMember e (cdr lst))))))


(let ((listB '(1 2 (3 8) (5 (6 7)))))
  (format t "b: ~a~%" (checkMember 4 listB)))

; c

(defun myAppend (list1 list2)
  (if (null list1)
      list2
      (cons (car list1) (myAppend (cdr list1) list2))))



; findSublists(l1l2..ln) = | nil, n == 0
;                          | findSublists(l1) U findSublists(l2l3..ln), if l1 is a sublist
;                          | findSublists(l2l3..ln), otherwise


(defun findSublists (lst)
  (if (null lst)
      nil
      (if (listp (car lst))
          (myAppend (car lst)(list (myAppend (findSublists (car lst)) (findSublists (cdr lst)))))
          (findSublists (cdr lst)))))

(let ((listC '(1 2 (3 (4 5) (6 7)) 8 (9 10))))
  (format t "c: ~a~%" (findSublists (list listC))))


; d

; memb(e,l1l2..ln) = | nil, n == 0
;                    | true, e == l1
;                    | memb(l2l3..ln), otherwise

(defun memb (e lst)
  (if (null lst)
      nil
      (if (equal e (car lst))
          t
          (memb e (cdr lst)))))

; transformSet(l1l2..ln) = | nil, n == 0
;                          | transformSet(l2l3..ln), if memb(e l2l3..ln)
;                          | l1 U transfromSet(l2l3..ln), otherwise


(defun transformSet (lst)
  (if (null lst)
      nil
      (if (memb (car lst) (cdr lst))
          (transformSet (cdr lst))
          (cons (car lst)
                (transformSet (cdr lst))))))

(let ((listD '(1 2 3 2 4 5 4 5 6)))
  (format t "d: ~a~%" (transformSet listD)))