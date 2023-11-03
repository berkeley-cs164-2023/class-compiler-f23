The in-class compiler status after implementing anonymous functions.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

```
compile_and_run "(define (range lo hi)
  (if (< lo hi)
   (pair lo (range (add1 lo) hi))
   false))
(define (map f l)
  (if (not l) l
  (pair (f (left l)) (map f (right l)))))
(print (map (lambda (x) (+ x 1)) (range 0 4)))";;
```

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
