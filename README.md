The in-class compiler status after the class session on errors.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

`compile_and_run "(add1 false)";;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
