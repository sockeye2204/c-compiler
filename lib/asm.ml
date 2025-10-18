type reg = AX | DX | R10 | R11

type cond_code = E | NE | G | GE | L | LE

type operand = Imm of int | Register of reg | Pseudo of string | Stack of int

type unary_operator = Neg | Not

type binary_operator = Add | Sub | Mult

type instruction = Mov of operand * operand
                | Unary of {unary_operator: unary_operator; operand: operand}
                | Binary of {binary_operator: binary_operator; operand1: operand; operand2: operand}
                | Cmp of {operand1: operand; operand2: operand}
                | Idiv of operand
                | Cdq
                | Jmp of string
                | JmpCC of {cond_code: cond_code; identifier: string}
                | SetCC of {cond_code: cond_code; operand: operand}
                | Label of string
                | AllocateStack of int
                | Ret

type function_def = Function of {name: string; instructions: instruction list}

type t = Program of function_def