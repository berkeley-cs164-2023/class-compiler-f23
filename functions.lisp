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