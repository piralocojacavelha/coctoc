# coctoc

Fast, simple and minimal CoC (Calculus of Constructions) compiler that generates C code.

This project is still under active development, bugs are to be expected, this currently is not written with any care for memory safety or lack of undefined behavior.

# Code Gen

The compiler emits C code into an `out.c`, and turns the lambda terms into static inline void functions, which hopefully your C compiler can inline them, given they should always form an Acyclic Control Flow Graph, just from the calculus' type system.


