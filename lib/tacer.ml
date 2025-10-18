open Tempids

let convert_binary_op = function
  | Ast.Add -> Tac.Add
  | Ast.Subtract -> Tac.Subtract
  | Ast.Multiply -> Tac.Multiply
  | Ast.Divide -> Tac.Divide
  | Ast.Modulo -> Tac.Modulo
  | Ast.EqualTo -> Tac.EqualTo
  | Ast.NotEqualTo -> Tac.NotEqualTo
  | Ast.LessThan -> Tac.LessThan
  | Ast.LessThanOrEqualTo -> Tac.LessThanOrEqualTo
  | Ast.GreaterThan -> Tac.GreaterThan
  | Ast.GreaterThanOrEqualTo -> Tac.GreaterThanOrEqualTo
  | Ast.And | Ast.Or -> failwith "Can't convert Ast.And nor Ast.Or"

let convert_unary_op = function
  | Ast.Complement -> Tac.Complement
  | Ast.Negate -> Tac.Negate
  | Ast.Not -> Tac.Not

  let rec convert_exp = function
  | Ast.Constant c ->
      ([], Tac.Constant c)
  | Ast.Var v ->
      ([], Tac.Var v)
  | Ast.Unary { unary_operator = unaryop; expression = inner_section } ->
      let instructions, src = convert_exp inner_section in
      let dst_name = make_temp_id () in
      let dst = Tac.Var dst_name in
      let tacop = convert_unary_op unaryop in
      let newinstruction = Tac.Unary { unary_operator = tacop; src; dst } in
      (instructions @ [newinstruction], dst)
  | Ast.Binary{binary_operator=Ast.And; expression1=exp1; expression2=exp2} ->
      let instructions1, src1 = convert_exp exp1 in
      let instructions2, src2 = convert_exp exp2 in
      let false_label = make_label "and_false" in
      let end_label = make_label "and_end" in
      let dst_name = make_temp_id() in
      let dst = Tac.Var dst_name in
      let instructions = 
        instructions1
        @ [Tac.JumpIfZero{condition=src1; target=false_label}]
        @ instructions2
        @ [
          Tac.JumpIfZero{condition=src2; target=false_label};
          Tac.Copy{src = Tac.Constant 1; dst};
          Tac.Jump{target=end_label};
          Tac.Label false_label;
          Tac.Copy{src = Tac.Constant 0; dst};
          Tac.Label end_label;
        ]
      in
      (instructions, dst)
  | Ast.Binary{binary_operator=Ast.Or; expression1=exp1; expression2=exp2} ->
      let instructions1, src1 = convert_exp exp1 in
      let instructions2, src2 = convert_exp exp2 in
      let true_label = make_label "or_true" in
      let end_label = make_label "or_end" in
      let dst_name = make_temp_id() in
      let dst = Tac.Var dst_name in
      let instructions = 
        instructions1
        @ [Tac.JumpIfNotZero{condition=src1; target=true_label}]
        @ instructions2
        @ [
          Tac.JumpIfNotZero{condition=src2; target=true_label};
          Tac.Copy{src = Tac.Constant 0; dst};
          Tac.Jump{target=end_label};
          Tac.Label true_label;
          Tac.Copy{src = Tac.Constant 1; dst};
          Tac.Label end_label;
        ]
      in
      (instructions, dst)
  | Ast.Binary { binary_operator = binaryop; expression1 = exp1; expression2 = exp2 } ->
      let instructions1, src1 = convert_exp exp1 in
      let instructions2, src2 = convert_exp exp2 in
      let dst_name = make_temp_id () in
      let dst = Tac.Var dst_name in
      let tacop = convert_binary_op binaryop in
      let newinstruction = Tac.Binary { binary_operator = tacop; src1; src2; dst } in
      (instructions1 @ instructions2 @ [newinstruction], dst)
  | Ast.Assignment {expression1=Ast.Var v; expression2} ->
    let instructions1, result = convert_exp expression2 in
    let instructions =
      instructions1 @ [Tac.Copy{src = result; dst = Tac.Var v}]
    in
    (instructions, Tac.Var v)
  | Ast.Assignment _ -> failwith "Invalid lvalue for assignment!!!"



let convert_statement stmt =
  match stmt with
  | Ast.Return e ->
      let result = convert_exp e in
      let exp = fst result in
      let v = snd result in
      exp @ [Tac.Return v]
  | Ast.Expression e ->
      let result, _er = convert_exp e in
      result
  | Ast.Null -> []

let convert_block_item = function
  | Ast.S stmt -> convert_statement stmt
  | Ast.D (Ast.Declaration {name; init = Some exp}) ->
      let evaluate_assignment, _er = convert_exp (Ast.Assignment{expression1=Ast.Var name; expression2=exp}) in
      evaluate_assignment
  | Ast.D (Ast.Declaration{init = None; _}) -> []

let convert_function (Ast.Function {name; body}) =
  let instructions = List.concat_map convert_block_item body in
  let extra_return = Tac.Return(Constant 0) in
  Tac.Function{name; instructions= instructions @ [extra_return]}

let tacer (Ast.Program function_def) = Tac.Program (convert_function function_def)