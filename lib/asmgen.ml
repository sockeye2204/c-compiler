let convert_value = function
  | (Tac.Constant i) -> Asm.Imm i
  | (Tac.Var v) -> Asm.Pseudo v

let convert_unaryop = function
  | Tac.Negate -> Asm.Neg
  | Tac.Complement -> Asm.Not

let convert_instruction = function
  | Tac.Return value ->
    let v = convert_value value in
    Asm.[Mov(v, (Asm.Register AX)); Asm.Ret]
  | Tac.Unary {unary_operator=unaryop; src; dst} ->
    let asm_unaryop = convert_unaryop unaryop in
    let asm_src = convert_value src in
    let asm_dst = convert_value dst in
    Asm.[Mov(asm_src, asm_dst); Unary{unary_operator=asm_unaryop; operand=asm_dst}]
  | _ -> failwith "Todo"


let convert_function (Tac.Function {name; instructions}) =
  let instructions_conv = List.concat_map convert_instruction instructions in
  Asm.Function{name; instructions=instructions_conv}

let asmgen (Tac.Program function_def) = Asm.Program (convert_function function_def)