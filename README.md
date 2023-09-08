The in-class compiler status after the class session on the definitional interpreter and adding Booleans to the interpreter.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Interp;;`

`interp "(num? (not false))";;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
