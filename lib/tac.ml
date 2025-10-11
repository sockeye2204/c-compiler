type binary_operator = Add | Subtract | Multiply | Divide | Modulo | EqualTo | NotEqualTo | LessThan | LessThanOrEqualTo
  | GreaterThan | GreaterThanOrEqualTo
type unary_operator = Complement | Negate | Not [@@deriving show]
type value = Constant of int
           | Var of string [@@deriving show]

type instruction = Return of value
                | Unary of {unary_operator: unary_operator; src: value; dst: value}
                | Binary of {binary_operator: binary_operator; src1: value; src2: value; dst: value} 
                | Copy of {src: value; dst: value} 
                | Jump of {target: string}
                | JumpIfZero of {condition: value; target: string}
                | JumpIfNotZero of {condition: value; target: string}
                | Label of string [@@deriving show]

type function_def = Function of {name: string; instructions: instruction list} [@@deriving show]

type t = Program of function_def [@@deriving show]


