The in-class compiler status after the class session on tail calls.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

```
compile_and_run "(define (sum n total)
  (if (zero? n)
    total
    (sum (sub1 n) (+ n total))))
(print (sum 1000000 0))";;
```

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
