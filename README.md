The in-class compiler status after the class session on naming expressions (lets).  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

`compile_and_run "(+ (let ((x 1)) (add1 x)) (let ((y 2)) (add1 y)))";;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
