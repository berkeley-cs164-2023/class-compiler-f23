;; Example 1

(define (id x) x)
(print (id 4))

;; Example 2

(define (f x y) (+ x y))
(define (g x) (f x x))
(print (f 4 5))

;; Example 3

(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2)))))
(print (fib (read-num)))

;; Example 4

(define (even n) (if (zero? n) true (odd (sub1 n))))
(define (odd n) (if (zero? n) false (even (sub1 n))))
(print (even (read-num)))

;; Example 5.0

(define (sum n)
  (if (zero? n)
    n
    (+ n (sum (sub1 n)))))
(print (sum (read-num)))

;; Example 5.1

(define (sum n total)
  (if (zero? n)
    total
    (sum (sub1 n) (+ n total))))
(print (sum (read-num) 0))

;; Example 6

(define (f x) (+ 3 x))
(define (sum-f n total)
  (if (zero? n)
    total
    (sum-f (sub1 n) (+ (f n) total))))
(print (sum-f (read-num) 0))