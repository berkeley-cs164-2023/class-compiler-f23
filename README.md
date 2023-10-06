The in-class compiler status after the class session on producing output.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Compile;;`

`compile_and_run_io "(do (print (pair (read-num) (read-num))) (newline) (print 5))" "3\n4";;`

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
