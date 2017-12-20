; Jarrod Dunne
; Sample Sorting Algorithms

; Removes the first occurence of a value from an list
(defun remove-val (lst val)
  (if (= val (car lst))
    (cdr lst)
    (cons (car lst) (remove-val (cdr lst) val))))

; Generates a list from the result of calling lmbda n times
(defun repeat (lmbda n)
  (if (= 0 n)
      nil
    (cons (funcall lmbda) (repeat lmbda (- n 1)))))

; Generates a random list of integers
(defun rand-list (max-int length)
  (repeat (lambda () (floor (random max-int))) length))

; Determines if a list is sorted
(defun sorted? (lst)
  (if (> 2 (list-length lst))
    t
    (and
     (<= (car lst) (cadr lst))
     (sorted? (cdr lst)))))


; Returns the list sorted using selection sort
(defun selection-sort (lst)
  (if (eq nil lst)
    nil
    (let ((min-val (reduce #'min lst)))
      (cons min-val (selection-sort (remove-val lst min-val))))))

; Insert the element into the sorted list
(defun insert-sorted (lst elem)
  (cond ((eq nil lst) (list elem))
        ((< (car lst) elem) (append (list (car lst)) (insert-sorted (cdr lst) elem)))
        (t (append (list elem) lst))))

; Returns the list sorted using insertion sort
(defun insertion-sort (lst)
  (if (eq nil lst)
    nil
    (insert-sorted (insertion-sort (cdr lst)) (car lst))))

; Fixes the first inversion in an list
(defun fix-inversion (lst)
  (if (> 1 (list-length lst))
    lst
    (if (<= (car lst) (cadr lst))
      (append (list (car lst)) (fix-inversion (cdr lst)))
      (append (list (cadr lst)) (list (car lst)) (cddr lst)))))

; Returns the list sorted using bubble sort
(defun bubble-sort (lst)
  (if (sorted? lst)
    lst
    (bubble-sort (fix-inversion lst))))

; Merges two sorted lists
(defun merge-lst (lst1 lst2)
  (cond
   ((eq nil lst1) lst2)
   ((eq nil lst2) lst1)
   ((< (car lst1) (car lst2)) (append (list (car lst1)) (merge-lst (cdr lst1) lst2)))
   (t (append (list (car lst2)) (merge-lst lst1 (cdr lst2))))))

; Takes first n elements of list
(defun first-n (lst n)
  (if (= n 0)
    nil
    (cons (car lst) (first-n (cdr lst) (- n 1)))))

; Splits list into two equal halves
(defun bisect (lst)
  (let ((length (list-length lst)))
    (cons (first-n lst (floor (/ length 2))) (list (nthcdr (floor (/ length 2)) lst)))))

; Returns the list sorted using merge-sort
(defun merge-sort (lst)
    (if (> 2 (list-length lst))
        lst
      (let ((halves (bisect lst)))
        (merge-lst (merge-sort (car halves)) (merge-sort (cadr halves))))))

; Gets a random pivot element from an list
(defun get-pivot (lst)
  (let ((idx (floor (random (list-length lst)))))
    (nth idx lst)))

; Returns a (left-lst right-lst) pivoted on pivot, without the first pivot element
(defun pivot-lst (lst pivot left right)
  (cond
   ((eq nil lst) (cons left right))
   ((<= (car lst) pivot) (pivot-lst (cdr lst) pivot (append (list (car lst)) left) right))
   (t (pivot-lst (cdr lst) pivot left (append (list (car lst)) right)))))

; Returns the list sorted using quicksort
(defun quick-sort (lst)
  (cond ((> 2 (list-length lst)) lst)
        (t (let*
               ((pivot (get-pivot lst))
               (halves (pivot-lst (remove-val lst pivot) pivot nil nil)))
             (append (quick-sort (car halves)) (list pivot) (quick-sort (cdr halves)))))))


(defun test-all (len)
  (let ((lst (rand-list (* 2 len) len)))
    (progn
      (print 'INSERTION)
      (time (insertion-sort lst))
      (print 'SELECTION)
      (time (selection-sort lst))
      (print 'MERGE)
      (time (merge-sort lst))
      (print 'QUICK)
      (time (quick-sort lst))
      (print 'BUBBLE)
      (time (bubble-sort lst))
      nil)))
