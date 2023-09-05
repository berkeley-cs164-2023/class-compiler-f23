The in-class compiler status before the "Unary Operations" lecture.  See lecture notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

`compile_and_run "56";;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
