The in-class compiler status after implementing function pointers.  See compiler implementation notes for much more detail.

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
(define (g x) (+ x 1))
(print (map g (range 0 4)))";;
```

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
