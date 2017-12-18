; Jarrod Dunne
; Sample Sorting Algorithms

; Removes the first occurence of a value from an array
(defun remove-val (arr val)
  (if (= val (car arr))
    (cdr arr)
    (cons (car arr) (remove-val (cdr arr) val))))

; Generates a list from the result of calling lmbda n times
(defun repeat (lmbda n)
  (if (= 0 n)
      nil
    (cons (funcall lmbda) (repeat lmbda (- n 1)))))

; Generates a random list of integers
(defun rand-list (max-int length)
  (repeat (lambda () (floor (random max-int))) length))

; Reutnrs the array sorted using insertion sort
(defun insertion-sort (arr)
  (if (eq nil arr)
    nil
    (let ((min-val (reduce #'min arr)))
      (cons min-val (insertion-sort (remove-val arr min-val))))))