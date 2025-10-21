# C Compiler
This is a compiler frontend written in OCaml that converts C to x64 assembly. 

Why OCaml? Well, it has sum types and pattern matching which is useful when language constructs like 'expressions' can be defined in many different ways. You should be able to run the code through `dune build`. If you're on macOS, you'll probably need to make use of Rosetta 2.

## Current feature set

### Compiler stages

 1. Lexer: tokenisation of C code
 2. Parser: Building of a primitive abstract syntax tree (AST) from tokens
 3. Resolve: Resolve symbol names (semantic analysis)
 4. TACer: Convert the AST into [three-address code](https://en.wikipedia.org/wiki/Three-address_code)
 5. ASM generation: Convert the TAC AST into an assembly AST
 6. ASM emission: an assembly file is created through the assembly AST

After assembly has been generated, it is fed into gcc by invoking `gcc -m64 -arch x86_64` to handle assembling into an executable.

### Variables and datatypes

 - Local variable declaration
 - Datatypes (int)

### Control flow
 - If statement
 - Conditional operator

### Operators
 - Unary operators (Complement, Negate, Not)
 - Arithmetic binary operators (Add, Subtract, Multiply, Divide, Remainder)
 - Logical binary operators (And, Or)
 - Comparison binary operators (Equal, Not Equal, Less Than, Greater Than, Less Than Or Equal, Greater Than Or Equal)
 - Assignment operator
 - Compound assignment operator (Compound Add, Compound Subtract, Compound Multiply, Compound Divide, Compound Remainder)
 - Increment and decrement

## What's next?

Goto and labels.