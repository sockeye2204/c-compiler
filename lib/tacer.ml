open Tempids

let simplify_compound_op = function
  | Ast.CompoundAddition -> Ast.Add
  | Ast.CompoundSubtraction -> Ast.Subtract
  | Ast.CompoundMultiplication -> Ast.Multiply
  | Ast.CompoundDivision -> Ast.Divide
  | Ast.CompoundRemainder -> Ast.Modulo
  | Ast.PrefixIncrement -> Ast.Add
  | Ast.PrefixDecrement -> Ast.Subtract
  | Ast.PostfixIncrement -> Ast.Add
  | Ast.PostfixDecrement -> Ast.Subtract

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
  | Ast.Binary { binary_operator = binaryop; expression1 = exp1; expression2 = exp2 } ->
    (match binaryop with
      | Ast.And ->
        let instructions1, src1 = convert_exp exp1 in
        let instructions2, src2 = convert_exp exp2 in
        let false_label = make_label "and_false" in
        let end_label = make_label "and_end" in
        let dst_name = make_temp_id () in
        let dst = Tac.Var dst_name in
        let instructions =
          instructions1
          @ [Tac.JumpIfZero {condition=src1; target=false_label}]
          @ instructions2
          @ [
            Tac.JumpIfZero {condition=src2; target=false_label};
            Tac.Copy {src = Tac.Constant 1; dst};
            Tac.Jump {target=end_label};
            Tac.Label false_label;
            Tac.Copy {src = Tac.Constant 0; dst};
            Tac.Label end_label;
          ]
        in
        (instructions, dst)
      | Ast.Or ->
        let instructions1, src1 = convert_exp exp1 in
        let instructions2, src2 = convert_exp exp2 in
        let true_label = make_label "or_true" in
        let end_label = make_label "or_end" in
        let dst_name = make_temp_id () in
        let dst = Tac.Var dst_name in
        let instructions =
          instructions1
          @ [Tac.JumpIfNotZero {condition=src1; target=true_label}]
          @ instructions2
          @ [
            Tac.JumpIfNotZero {condition=src2; target=true_label};
            Tac.Copy {src = Tac.Constant 0; dst};
            Tac.Jump {target=end_label};
            Tac.Label true_label;
            Tac.Copy {src = Tac.Constant 1; dst};
            Tac.Label end_label;
          ]
        in
        (instructions, dst)
      | _ ->
        let instructions1, src1 = convert_exp exp1 in
        let instructions2, src2 = convert_exp exp2 in
        let dst_name = make_temp_id () in
        let dst = Tac.Var dst_name in
        let tacop = convert_binary_op binaryop in
        let newinstruction = Tac.Binary { binary_operator = tacop; src1; src2; dst } in
        (instructions1 @ instructions2 @ [newinstruction], dst))
  | Ast.Assignment {expression1=Ast.Var v; expression2; compound_operator} ->
    (match compound_operator with
    | Some op ->
      (match op with
      | CompoundAddition | CompoundSubtraction | CompoundMultiplication
      | CompoundDivision | CompoundRemainder ->
        let tacop = simplify_compound_op op in
        let simplified =
          Ast.Assignment {expression1 = Ast.Var v;
          expression2 = Ast.Binary { binary_operator = tacop; expression1 = Ast.Var v; expression2 = expression2};
          compound_operator = None} in
        let newinstruction, dst = convert_exp simplified in
        (newinstruction, dst)
      | PrefixIncrement | PrefixDecrement ->
        let tacop = simplify_compound_op op in
        let simplified =
          Ast.Assignment {expression1 = Ast.Var v;
          expression2 = Ast.Binary { binary_operator = tacop; expression1 = Ast.Var v; expression2 = Ast.Constant 1 };
          compound_operator = None} in
        let crementbyone, dst = convert_exp simplified in
        (crementbyone, dst)      
      | PostfixIncrement | PostfixDecrement ->
        let tacop = simplify_compound_op op in
        let simplified =
          Ast.Assignment {expression1 = Ast.Var v;
          expression2 = Ast.Binary { binary_operator = tacop; expression1 = Ast.Var v; expression2 = Ast.Constant 1 };
          compound_operator = None} in
        let crementbyone, _ = convert_exp simplified in
        let dst_name = make_temp_id () in
        let dst = Tac.Var dst_name in
        let instructions =
          [Tac.Copy {src = Tac.Var v; dst };]
          @ crementbyone
        in
        (instructions, dst))    
    | None ->
      let instructions1, result = convert_exp expression2 in
      let instructions =
        instructions1 @ [Tac.Copy {src = result; dst = Tac.Var v}]
      in
      (instructions, Tac.Var v))
  | Ast.Assignment _ -> failwith "Invalid lvalue for assignment!!!"
  | Ast.Conditional { condition; expression1; expression2 } ->
    let instructions_cond, src_cond = convert_exp condition in
    let instructions_expression1, src_exp1 = convert_exp expression1 in
    let instructions_expression2, src_exp2 = convert_exp expression2 in
    let end_label = make_label "end" in
    let else_label = make_label "else_label" in
    let dst_name = make_temp_id () in
    let dst = Tac.Var dst_name in
    let instructions =
    instructions_cond
    @ [Tac.JumpIfZero {condition=src_cond; target=else_label}]
    @ instructions_expression1
    @ [
      Tac.Copy {src = src_exp1; dst = dst};
      Tac.Jump {target=end_label};
      ]
    @ [Tac.Label else_label]
    @ instructions_expression2
    @ [
      Tac.Copy {src = src_exp2; dst = dst};
      ]
    @ [Tac.Label end_label]
    in
    (instructions, dst)


let rec convert_statement stmt =
  match stmt with
  | Ast.If { condition; thenb; elseb } ->
    let instructions_cond, src_cond = convert_exp condition in
    let instructions_thenb = convert_statement thenb in
    let end_label = make_label "end" in
    (match elseb with
      | Some stmt ->
        let instructions_elseb = convert_statement stmt in
        let else_label = make_label "else_label" in
        let instructions =
        instructions_cond
        @ [Tac.JumpIfZero {condition=src_cond; target=else_label}]
        @ instructions_thenb
        @ [Tac.Jump {target=end_label}]
        @ [Tac.Label else_label]
        @ instructions_elseb
        @ [Tac.Label end_label]
        in
        instructions
      | None ->
        let instructions =
        instructions_cond
        @ [Tac.JumpIfZero {condition=src_cond; target=end_label}]
        @ instructions_thenb
        @ [Tac.Label end_label];
        in
        instructions
    )
  | Ast.Return e ->
    let result = convert_exp e in
    let exp = fst result in
    let v = snd result in
    exp @ [Tac.Return v]
  | Ast.Expression e ->
    let result, _er = convert_exp e in
    result
  | Ast.Goto {target} -> [Tac.Jump {target}]
  | Ast.Label name -> [Tac.Label name]
  | Ast.Null -> []

let convert_block_item = function
  | Ast.S stmt -> convert_statement stmt
  | Ast.D (Ast.Declaration {name; init = Some exp}) ->
    let evaluate_assignment, _er = convert_exp (Ast.Assignment {expression1=Ast.Var name; expression2=exp; compound_operator=None}) in
    evaluate_assignment
  | Ast.D (Ast.Declaration {init = None; _}) -> []

let convert_function (Ast.Function {name; body}) =
  let instructions = List.concat_map convert_block_item body in
  let extra_return = Tac.Return (Constant 0) in
  Tac.Function {name; instructions = instructions @ [extra_return]}

let tacer (Ast.Program function_def) = Tac.Program (convert_function function_def)