(define (f g) (g 2))
(define (mul2 x) (+ x x))
(print (f mul2))

(define (f g) (g 2))
(print (f (lambda (x) (+ x x))))

(define (f g) (g 2))
(let ((y 3)) (print (f (lambda (x) (+ x y)))))