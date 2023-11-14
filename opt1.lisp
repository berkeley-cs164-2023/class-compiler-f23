(define (f x)
    (+ x 2))
(print (* (f (read-num)) (f (read-num))))

















(define (g x)
    (* x 3))
(define (f x)
    (+ (g x) 2))
(print (* (f (read-num)) (f (read-num))))


















(define (map f l)
    (if (empty? l) ())
        (pair (f (left l)) (map f (right l))))
(define (f x)
    (+ (g x) 2))
(print (map f (pair 1 (pair 2 ())))) 