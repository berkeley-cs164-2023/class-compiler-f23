The in-class compiler status after the "Unary Operations" class session.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

`compile_and_run "(sub1 (sub1 (sub1 (add1 50))))";;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
