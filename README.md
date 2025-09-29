=== C Compiler ===

This is a compiler frontend written in OCaml that converts C to x64 assembly. Why OCaml? Well, it has sum types and pattern matching which is useful when language constructs like 'expressions' can be defined in many different ways. You should be able to run the code by using ```dune build```. If you're on macOS like me, you'll probably need to make use of Rosetta 2.