(let ((x (read-num)))  
    (+ (* (+ x 2) (+ x 2)) (+ x 2)))















(define (sum-to x)  
    (if (= x 0) 0    
        (+ x (sum-to (sub1 x)))))
(let ((x (read-num)))
    (+ (* (sum-to x) (sum-to x)) (sum-to x)))














(define (read-plus x)
    (+ (read-num) x))
(let ((x (read-num)))
    (+ (* (read-plus x) (read-plus x)) (read-plus x))) 