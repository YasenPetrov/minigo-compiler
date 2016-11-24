* How to build the source files:
  - run ocamlbuild calc.native

* How to compile Mini-Go code:
  - Place your code in a file called text.txt
  - run ./calc.native
  - The following will bu output to the console:
    + The AST after Parsing
    + The AST after renaming all variables (to be unique)
    + The AST after typechecking
    + The activation record for the main body
    + The Intermediate language code
    + The VM code
    + The output of the VM code

* A sample program is provided in a file called test.txt

* Problems:
  - I have not tested extensively, but the main components seem to work fine.
    More work is needed on error reporting.
