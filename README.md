The in-class compiler status after the second class session on parsing.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

`test ();;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
