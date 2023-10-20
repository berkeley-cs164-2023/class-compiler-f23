The in-class compiler status after the class session on tail calls.  See compiler implementation notes for much more detail.

To run:

`dune utop`

Inside utop:

`open Cs164.Handparser;;`

```
parse "( + 1 ( + 2 3 ) )";;
```

And remember that you must recompile the runtime if you make changes:

`gcc -c runtime.c -o runtime.o`
