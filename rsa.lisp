(defun modexp (x y n)
  (if (= 0 y)
   1
   (let ((z (modexp x (floor (/ y 2)) n)))
     (if (= 0 (mod y 2))
       (mod (* z z) n)
       (mod (* x z z) n)))))

(defun euclid (a b)
  (if (= 0 b)
    a
    (euclid b (mod a b))))

(defun extended-euclid (a b)
  (if (= 0 b)
    (list 1 0 a)
    (let* ((vals (extended-euclid b (mod a b)))
          (x (car vals))
          (y (cadr vals))
          (d (caddr vals)))
      (list y (- x (* y (floor (/ a b)))) d))))

(defun primality-k (n k)
  (= 1 (modexp k (- n 1) n)))

(defun primality-rand (n)
  (primality-k (n (+ 1 (floor (random (- n 2)))))))

(defun repeat (lmbda n)
  (if (= 0 n)
      nil
    (cons (funcall lmbda) (repeat lmbda (- n 1)))))

(defun primality-2-3-5 (n)
  (and
   (primality-k n 2)
   (primality-k n 3)
   (primality-k n 5)))

(defun primality-100 (n)
  (reduce (lambda (a b) (and a b)) (repeat (lambda () (primality n)) 100) :initial-value t))

(defun get-prime (bits)
  (let ((a (+ 1 (floor (random (- (expt 2 bits) 1))))))
    (if (primality-2-3-5 a)
      a
      (get-prime bits))))

(defun priv-key (key)
  (car key))

(defun pub-key (key)
  (cdr key))

(defun generate-key (bits)
  (let ((p (get-prime bits)) (q (get-prime bits)))
    (list
     (caddr (extended-euclid 3 (* (- p 1) (- q 1))))
     (* p q)
     3)))

(defun encode (msg pub-key)
  (modexp msg (cadr pub-key) (car pub-key)))

(defun decode (encoded-msg priv-key pub-key)
  (modexp encoded-msg priv-key (car pub-key)))

(defun rand-msg (bits)
  (floor (random (- (expt 2 bits) 1))))

(defun encode-decode (bits)
  (let ((msg (rand-msg bits)) (key (generate-key bits)))
    (decode (encode msg (pub-key key)) (priv-key key) (pub-key key))))

(defun time-rsa (bits up-to)
  (if (= bits up-to)
    nil
    (progn
     (print bits)
     (time (encode-decode bits))
     (time-rsa (+ 1 bits) up-to))))
