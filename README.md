The in-class compiler status after implementing closures.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

```
compile_and_run "(define (f g) (g 2))
(let ((y 3)) (print (f (lambda (x) (+ x y)))))";;
```

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
