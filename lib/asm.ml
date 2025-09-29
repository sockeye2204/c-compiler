type reg = AX | R10
type operand = Imm of int | Register of reg | Pseudo of string | Stack of int
type unary_operator = Neg | Not
type instruction = Mov of operand * operand
                  | Unary of {unary_operator: unary_operator; operand: operand}
                  | AllocateStack of int
                  | Ret
type function_def = Function of {name: string; instructions: instruction list}
type t = Program of function_def