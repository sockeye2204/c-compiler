type unary_operator = Complement | Negate [@@deriving show]
type binary_operator = Add | Subtract | Multiply | Divide | Modulo [@@deriving show]
type expression = Constant of int
                | Unary of {unary_operator: unary_operator; expression: expression}
                | Binary of {binary_operator: binary_operator; expression1: expression; expression2: expression} [@@deriving show]
type statement = Return of expression [@@deriving show]

type function_def = Function of {name: string; body: statement} [@@deriving show]

type t = Program of function_def [@@deriving show]