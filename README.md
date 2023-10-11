The in-class compiler status after the class session on implementing functions in the compiler.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Interp;;`

`compile_and_run "(define (fib n) (if (< n 2) n (+ (fib (- n 1)) (fib (- n 2))))) (print (fib 20))";;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
