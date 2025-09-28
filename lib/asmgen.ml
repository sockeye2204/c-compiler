let rec convert_exp = function
  | (Ast.Constant i) -> Asm.Imm i
  | Ast.Unary {unary_operator = _; expression} -> convert_exp expression

let convert_statement (Ast.Return exp) =
  let v = convert_exp exp in
  Asm.[Mov(v, Asm.Register); Asm.Ret]

let convert_function (Ast.Function {name; body}) =
  Asm.Function{name; instructions = convert_statement body}

let asmgen (Ast.Program function_def) = Asm.Program (convert_function function_def)