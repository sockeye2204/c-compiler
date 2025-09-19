type expression = Constant of int [@@deriving show]
type statement = Return of expression [@@deriving show]

type function_def = Function of {name: string; body: statement} [@@deriving show]

type t = Program of function_def [@@deriving show]