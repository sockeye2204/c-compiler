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
[@@deriving show]