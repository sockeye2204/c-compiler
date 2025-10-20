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
  | Increment
  | Decrement
  | Negation
  | BWComplement
  | Addition
  | Multiplication
  | Division
  | Remainder
  | LogicalNot
  | LogicalAnd
  | LogicalOr
  | EqualTo
  | NotEqualTo
  | LessThan
  | GreaterThan
  | LessThanOrEqualTo
  | GreaterThanOrEqualTo
  | Assignment
  | CompoundAddition
  | CompoundSubtraction
  | CompoundMultiplication
  | CompoundDivision
  | CompoundRemainder
[@@deriving show]