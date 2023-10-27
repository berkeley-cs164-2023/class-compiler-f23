The in-class compiler status before the second class session on parsing.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Handparser2;;`

`parse "2 + 3 * 10";;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
