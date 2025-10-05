open Tempids

let convert_binary_op = function
  | Ast.Add -> Tac.Add
  | Ast.Subtract -> Tac.Subtract
  | Ast.Multiply -> Tac.Multiply
  | Ast.Divide -> Tac.Divide
  | Ast.Modulo -> Tac.Modulo

let convert_unary_op = function
  | Ast.Complement -> Tac.Complement
  | Ast.Negate -> Tac.Negate

let rec convert_exp = function
  | Ast.Constant c -> ([], Tac.Constant c)
  | Ast.Unary{unary_operator=unaryop; expression=inner_section} ->
    let (instructions, src) = convert_exp(inner_section) in
    let dst_name = make_temp_id() in
    let dst = Tac.Var(dst_name) in
    let tacop = convert_unary_op(unaryop) in
    let newinstruction = Tac.Unary{unary_operator=tacop; src; dst} in
    let instructions = List.append instructions [newinstruction] in
      (instructions, dst)
  | Ast.Binary{binary_operator=binaryop; expression1=exp1; expression2=exp2} ->
    let (instructions1, src1) = convert_exp(exp1) in
    let (instructions2, src2) = convert_exp(exp2) in
    let dst_name = make_temp_id() in
    let dst = Tac.Var(dst_name) in
    let tacop = convert_binary_op(binaryop) in
    let newinstruction = Tac.Binary{binary_operator=tacop; src1; src2; dst} in
    let instructions = List.append (List.append instructions1 instructions2) [newinstruction] in
      (instructions, dst)


let convert_statement stmt =
  match stmt with
  | Ast.Return e ->
      let result = convert_exp e in
      let exp = fst result in
      let v = snd result in
      exp @ [Tac.Return v]

let convert_function (Ast.Function {name; body}) =
  Tac.Function{name; instructions = convert_statement body}

let tacer (Ast.Program function_def) = Tac.Program (convert_function function_def)