; Jarrod Dunne
; Simple RSA Encryption/Decryption

; Computes x^y (mod n)
(defun modexp (x y n)
  (if (= 0 y)
   1
   (let ((z (modexp x (floor (/ y 2)) n)))
     (if (= 0 (mod y 2))
       (mod (* z z) n)
       (mod (* x z z) n)))))

; Computes gcd(a b)
(defun euclid (a b)
  (if (= 0 b)
    a
    (euclid b (mod a b))))

; Compuetes gcd(a b), and x,y such that ax + by = gcd(a b)
(defun extended-euclid (a b)
  (if (= 0 b)
    (list 1 0 a)
    (let* ((vals (extended-euclid b (mod a b)))
          (x (car vals))
          (y (cadr vals))
          (d (caddr vals)))
      (list y (- x (* y (floor (/ a b)))) d))))

; Primality test using fermat's little theorem, that k^(n-1) (mod n) = 1
(defun primality-k (n k)
  (= 1 (modexp k (- n 1) n)))

; Primality test using fermat's little theorem on a random integer [1,n-1]
(defun primality-rand (n)
  (primality-k (n (+ 1 (floor (random (- n 2)))))))

; Generates a list from the result of calling lmbda n times
(defun repeat (lmbda n)
  (if (= 0 n)
      nil
    (cons (funcall lmbda) (repeat lmbda (- n 1)))))

; Tests with fermat's little theorem on 2, 3, and 5
(defun primality-2-3-5 (n)
  (and
   (primality-k n 2)
   (primality-k n 3)
   (primality-k n 5)))

; Tests with fermat's little theorem with 100 random numbers
(defun primality-100 (n)
  (reduce (lambda (a b) (and a b)) (repeat (lambda () (primality-rand n)) 100) :initial-value t))

; Gets a random 'prime' in [2, 2^bits-1]
(defun get-prime (bits)
  (let ((a (+ 1 (floor (random (- (expt 2 bits) 1))))))
    (if (primality-2-3-5 a)
      a
      (get-prime bits))))

; Generates a random private and public key for RSA, of form (priv-key, pub-key1, pub-key2)
(defun generate-key (bits)
  (let ((p (get-prime bits)) (q (get-prime bits)))
    (list
     (caddr (extended-euclid 3 (* (- p 1) (- q 1))))
     (* p q)
     3)))

; Gets private key from a RSA key triple
(defun priv-key (key)
  (car key))

; Gets public key pair from a RSA key triple
(defun pub-key (key)
  (cdr key))

; Encodes a msg (integer) using a public key
(defun encode (msg pub-key)
  (modexp msg (cadr pub-key) (car pub-key)))

; Decodes a msg (integer) using a private key and public key
(defun decode (encoded-msg priv-key pub-key)
  (modexp encoded-msg priv-key (car pub-key)))

; Generates a random msg (integer) in [0, 2^bits - 1]
(defun rand-msg (bits)
  (floor (random (- (expt 2 bits) 1))))

; Encodes and decodes a random message using n-bit RSA encryption
(defun encode-decode (bits)
  (let ((msg (rand-msg bits)) (key (generate-key bits)))
    (decode (encode msg (pub-key key)) (priv-key key) (pub-key key))))

; Times executions of RSA encode-decode cycles with [bits, up-to] bits successively
(defun time-rsa (bits up-to)
  (if (= bits up-to)
    nil
    (progn
     (print bits)
     (time (encode-decode bits))
     (time-rsa (+ 1 bits) up-to))))
