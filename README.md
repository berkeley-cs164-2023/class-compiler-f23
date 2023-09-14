The in-class compiler status after the first class session on adding Booleans to the compiler.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

`compile_and_run "true";;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
