type unary_operator = Complement | Negate | Not [@@deriving show]

type binary_operator = Add | Subtract | Multiply | Divide | Modulo | And | Or
                    | EqualTo | NotEqualTo | LessThan | GreaterThan | LessThanOrEqualTo | GreaterThanOrEqualTo [@@deriving show]

type compound_operator = CompoundAddition | CompoundSubtraction | CompoundMultiplication
                    | CompoundDivision | CompoundRemainder
                    | PrefixIncrement | PrefixDecrement 
                    | PostfixIncrement | PostfixDecrement [@@deriving show]

type expression = Constant of int
                | Var of string
                | Unary of {unary_operator: unary_operator; expression: expression}
                | Binary of {binary_operator: binary_operator; expression1: expression; expression2: expression}
                | Assignment of {expression1: expression; expression2: expression; compound_operator: compound_operator option} [@@deriving show]

type statement = Return of expression
              | Expression of expression
              | Null [@@deriving show]

type declaration = Declaration of {name: string; init: expression option} [@@deriving show]

type block_item = S of statement
                | D of declaration [@@deriving show]

type function_def = Function of {name: string; body: block_item list} [@@deriving show]

type t = Program of function_def [@@deriving show]