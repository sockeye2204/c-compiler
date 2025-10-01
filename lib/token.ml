type t =
| Identifier of string
| Constant of int
| KWInt
| KWVoid
| KWReturn
| ParenOpen
| ParenClose
| BraceOpen
| BraceClose
| Semicolon
| Decrement
| Negation
| BWComplement
| Addition
| Multiplication
| Division
| Remainder
[@@deriving show]