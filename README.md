The in-class compiler status after the class session on adding Boolean operations and conditionals to the compiler.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

`compile_and_run "(if (not true) 1 (if false 2 3))";;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
