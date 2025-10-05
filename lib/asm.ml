type reg = AX | DX | R10 | R11
type operand = Imm of int | Register of reg | Pseudo of string | Stack of int
type unary_operator = Neg | Not
type binary_operator = Add | Sub | Mult
type instruction = Mov of operand * operand
                  | Unary of {unary_operator: unary_operator; operand: operand}
                  | Binary of {binary_operator: binary_operator; operand1: operand; operand2: operand}
                  | Idiv of operand
                  | Cdq
                  | AllocateStack of int
                  | Ret
type function_def = Function of {name: string; instructions: instruction list}
type t = Program of function_def