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
    [
      Asm.Mov(v, (Asm.Register AX));
      Asm.Ret
    ]
  | Tac.Unary {unary_operator=unaryop; src; dst} -> (
    let asm_src = convert_value src in
    let asm_dst = convert_value dst in
    match unaryop with
    | Tac.Not ->
      [
        Asm.Cmp {operand1=Asm.Imm 0; operand2=asm_src};
        Asm.Mov (Asm.Imm 0, asm_dst);
        Asm.SetCC {cond_code=Asm.E; operand=asm_dst};
      ]
    | _ ->
      let asm_unaryop = convert_unaryop unaryop in
      [
        Asm.Mov(asm_src, asm_dst);
        Asm.Unary{unary_operator=asm_unaryop; operand=asm_dst}
      ])
  | Tac.Binary {binary_operator=binaryop; src1; src2; dst} -> (
    let asm_src1 = convert_value src1 in
    let asm_src2 = convert_value src2 in
    let asm_dst = convert_value dst in
    match binaryop with
    | Tac.EqualTo | Tac.NotEqualTo | Tac.GreaterThan
    | Tac.GreaterThanOrEqualTo | Tac.LessThan | Tac.LessThanOrEqualTo ->
      let asm_cond_code = if binaryop = Tac.EqualTo then Asm.E else
                      if binaryop = Tac.NotEqualTo then Asm.NE else
                      if binaryop = Tac.GreaterThan then Asm.G else
                      if binaryop = Tac.GreaterThanOrEqualTo then Asm.GE else
                      if binaryop = Tac.LessThan then Asm.L else
                      Asm.LE in
      [
        Asm.Cmp {operand1=asm_src2; operand2=asm_src1};
        Asm.Mov (Asm.Imm 0, asm_dst);
        Asm.SetCC {cond_code=asm_cond_code; operand=asm_dst};
      ]
    | Tac.Divide | Tac.Modulo ->
      let result_reg = if binaryop = Tac.Divide then Asm.AX else Asm.DX in
      [
        Asm.Mov (asm_src1, Asm.Register(Asm.AX));
        Asm.Cdq;
        Asm.Idiv (asm_src2);
        Asm.Mov (Asm.Register result_reg, asm_dst);
      ]
    | _ ->
      let asm_binaryop = convert_binaryop binaryop in
      [
        Asm.Mov (asm_src1, asm_dst);
        Asm.Binary {binary_operator = asm_binaryop; operand1=asm_src2; operand2=asm_dst};
      ])
  | Tac.Jump {target} ->
    [Asm.Jmp target]
  | Tac.JumpIfZero {condition; target} ->
    let asm_condition = convert_value condition in
    [
      Asm.Cmp{operand1=Asm.Imm 0; operand2=asm_condition};
      Asm.JmpCC{cond_code= E; identifier=target}
    ]
  | Tac.JumpIfNotZero {condition; target} ->
    let asm_condition = convert_value condition in 
    [
      Asm.Cmp{operand1=Asm.Imm 0; operand2=asm_condition};
      Asm.JmpCC{cond_code= NE; identifier=target}
    ]
  | Tac.Label label ->
    [Asm.Label label]
  | Tac.Copy {src; dst} ->
    let asm_src = convert_value src in
    let asm_dst = convert_value dst in
    [Asm.Mov (asm_src, asm_dst)]


let convert_function (Tac.Function {name; instructions}) =
  let instructions_conv = List.concat_map convert_instruction instructions in
  Asm.Function{name; instructions=instructions_conv}

let asmgen (Tac.Program function_def) = Asm.Program (convert_function function_def)