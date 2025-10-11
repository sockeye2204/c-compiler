let convert_value = function
  | (Tac.Constant i) -> Asm.Imm i
  | (Tac.Var v) -> Asm.Pseudo v

let convert_unaryop = function
  | Tac.Negate -> Asm.Neg
  | Tac.Complement -> Asm.Not
  | _ -> failwith "todo"

let convert_binaryop = function
  | Tac.Add -> Asm.Add
  | Tac.Subtract -> Asm.Sub
  | Tac.Multiply -> Asm.Mult
  | Tac.Divide | Tac.Modulo -> failwith "Cannot convert division or modulo operators"
  | _ -> failwith "todo"

let convert_instruction = function
  | Tac.Return value ->
    let v = convert_value value in
    Asm.[Mov(v, (Asm.Register AX)); Asm.Ret]
  | Tac.Unary {unary_operator=unaryop; src; dst} ->
    let asm_unaryop = convert_unaryop unaryop in
    let asm_src = convert_value src in
    let asm_dst = convert_value dst in
    Asm.[Mov(asm_src, asm_dst); Unary{unary_operator=asm_unaryop; operand=asm_dst}]
  | Tac.Binary {binary_operator=binaryop; src1; src2; dst} -> (
    let asm_src1 = convert_value src1 in
    let asm_src2 = convert_value src2 in
    let asm_dst = convert_value dst in
    match binaryop with
    | Tac.Divide | Tac.Modulo ->
      let result_reg = if binaryop = Divide then Asm.AX else Asm.DX in
      [
        Mov (asm_src1, Register(Asm.AX));
        Cdq;
        Idiv (asm_src2);
        Mov (Register result_reg, asm_dst);
      ]
    | _ ->
      let asm_binaryop = convert_binaryop binaryop in
      [
        Mov (asm_src1, asm_dst);
        Binary {binary_operator = asm_binaryop; operand1=asm_src2; operand2=asm_dst};
      ])
  | _ -> failwith "todo"


let convert_function (Tac.Function {name; instructions}) =
  let instructions_conv = List.concat_map convert_instruction instructions in
  Asm.Function{name; instructions=instructions_conv}

let asmgen (Tac.Program function_def) = Asm.Program (convert_function function_def)