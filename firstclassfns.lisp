(define (f g) (g 2))
(define (mul2 x) (+ x x))
(print (f mul2))

(define (mul2 x) (+ x x))
(let ((newname mul2)) (print (newname 5)))

(define (range lo hi)
  (if (< lo hi)
   (pair lo (range (add1 lo) hi))
   false))
(define (map f l)
  (if (not l) l
  (pair (f (left l)) (map f (right l)))))
(define (g x) (+ x 1))
(print (map g (range 0 4)))

-------

(define (f g) (g 2))
(print (f (lambda (x) (+ x x))))

(define (range lo hi)
  (if (< lo hi)
   (pair lo (range (add1 lo) hi))
   false))
(define (map f l)
  (if (not l) l
  (pair (f (left l)) (map f (right l)))))
(print (map (lambda (x) (+ x 1)) (range 0 4)))

-------

(define (f g) (g 2))
(let ((y 3)) (print (f (lambda (x) (+ x y)))))

(define (retlam)
    (let ((x 3))
        (lambda () x)
    )
)
(let ((l (retlam)))
    (print (l))
)

-------
for discussion

(define (f a) (+ a b))
(print (f 2))

(define (sum-to x)
  (if (= x 0) 
    0
    (+ x (sum-to (sub1 x)))
  ))
(print (sum-to 700))