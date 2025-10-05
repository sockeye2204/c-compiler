type binary_operator = Add | Subtract | Multiply | Divide | Modulo
type unary_operator = Complement | Negate [@@deriving show]
type value = Constant of int
           | Var of string [@@deriving show]

type instruction = Return of value
                | Unary of {unary_operator: unary_operator; src: value; dst: value}
                | Binary of {binary_operator: binary_operator; src1: value; src2: value; dst: value} [@@deriving show]

type function_def = Function of {name: string; instructions: instruction list} [@@deriving show]

type t = Program of function_def [@@deriving show]


