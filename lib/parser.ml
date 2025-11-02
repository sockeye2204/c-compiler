open Token
exception ParserError of string

module Private = struct
  let precedence = function
  | Token.Multiplication | Token.Division | Token.Remainder -> Some 30
  | Token.Addition | Token.Negation -> Some 25
  | Token.LessThan | Token.GreaterThan | Token.LessThanOrEqualTo | Token.GreaterThanOrEqualTo -> Some 20
  | Token.EqualTo | Token.NotEqualTo -> Some 15
  | Token.LogicalAnd -> Some 10
  | Token.LogicalOr -> Some 5
  | Token.CompoundAddition | Token.CompoundSubtraction | Token.CompoundMultiplication
  | Token.CompoundDivision | Token.CompoundRemainder
  | Token.QuestionMark -> Some 3
  | Token.Assignment -> Some 1
  | _ -> None

  let rec parse_id tokens =
    match tokens with
    | Identifier name :: rest -> (name, rest)
    | ParenOpen :: rest ->
      let (name, rest) = parse_id rest in
      (match rest with
        | ParenClose :: rest -> (name, rest)
        | _ -> raise (ParserError "Expected closing brace"))
    | _ -> raise (ParserError "Expected identifier name")

  let parse_int tokens =
    match tokens with
    | Constant n :: rest -> (Ast.Constant n, rest)
    | _ -> raise (ParserError "Expected constant")
  
  let parse_unary_op tokens =
    match tokens with
    | Token.BWComplement :: rest -> (Ast.Complement, rest)
    | Token.Negation :: rest -> (Ast.Negate, rest)
    | Token.LogicalNot :: rest -> (Ast.Not, rest)
    | _ -> raise (ParserError "Expected unary operator")
  
  let parse_binary_op tokens =
    match tokens with
    | Multiplication :: rest -> (Ast.Multiply, rest)
    | Division :: rest -> (Ast.Divide, rest)
    | Remainder :: rest -> (Ast.Modulo, rest)
    | Addition :: rest -> (Ast.Add, rest)
    | Negation :: rest -> (Ast.Subtract, rest)
    | LogicalAnd :: rest -> (Ast.And, rest)
    | LogicalOr :: rest -> (Ast.Or, rest)
    | EqualTo :: rest -> (Ast.EqualTo, rest)
    | NotEqualTo :: rest -> (Ast.NotEqualTo, rest)
    | LessThan :: rest -> (Ast.LessThan, rest)
    | GreaterThan :: rest -> (Ast.GreaterThan, rest)
    | LessThanOrEqualTo :: rest -> (Ast.LessThanOrEqualTo, rest)
    | GreaterThanOrEqualTo :: rest -> (Ast.GreaterThanOrEqualTo, rest)
    | _ -> raise (ParserError "Expected binary operator")

  let rec parse_expression min_prec tokens =
    let (left, remaining) = parse_factor tokens in
    let rec p_e_while left remaining =
      match remaining with
      | [] -> (left, remaining)
      | next :: _ ->
        match precedence next with
        | Some prec when prec >= min_prec ->
          (match next with
           | Token.CompoundAddition | Token.CompoundSubtraction | Token.CompoundMultiplication
           | Token.CompoundDivision | Token.CompoundRemainder | Token.Assignment ->
             let remaining2 = List.tl remaining in
             let (right, remaining3) = parse_expression prec remaining2 in
             let compoundop = 
              match next with
              | Token.CompoundAddition -> Some Ast.CompoundAddition
              | Token.CompoundSubtraction -> Some Ast.CompoundSubtraction
              | Token.CompoundMultiplication -> Some Ast.CompoundMultiplication
              | Token.CompoundDivision -> Some Ast.CompoundDivision
              | Token.CompoundRemainder -> Some Ast.CompoundRemainder
              | Token.Assignment -> None
              | _ -> failwith "Invalid assignment operator for compound operator"
             in
             let left = Ast.Assignment { expression1 = left; expression2 = right; compound_operator = compoundop } in
             p_e_while left remaining3
           | Token.QuestionMark ->
            let (middle, remaining) = parse_conditional_middle remaining in
            let (right, remaining) = parse_expression prec remaining in
            let left = Ast.Conditional { condition = left; expression1 = middle; expression2 = right} in
            p_e_while left remaining
           | _ ->
             let (operator, remaining2) = parse_binary_op remaining in
             let (right, remaining3) = parse_expression (prec + 1) remaining2 in
             let left = Ast.Binary { binary_operator = operator; expression1 = left; expression2 = right } in
             p_e_while left remaining3)
        | _ -> (left, remaining)
    in
    p_e_while left remaining

  and parse_factor tokens =
  
    let parse_primary tokens =
      match tokens with
      | Constant _ :: _ -> parse_int tokens
      | Identifier name :: rest -> 
        (Ast.Var name, rest)
      | ParenOpen :: rest ->
        let (expr, remaining) = parse_expression 0 rest in
        (match remaining with
        | ParenClose :: rest -> (expr, rest)
        | _ -> raise (ParserError "Expected closing brace"))
      | _ -> raise (ParserError "Expected factor")
    in

    let rec parse_postfix left tokens =
      match tokens with
      | Increment :: rest ->
        let new_expr = Ast.Assignment {expression1 = left; expression2 = Ast.Constant 1; compound_operator = Some Ast.PostfixIncrement} in
        parse_postfix new_expr rest
      | Decrement :: rest ->
        let new_expr = Ast.Assignment {expression1 = left; expression2 = Ast.Constant 1; compound_operator = Some Ast.PostfixDecrement} in
        parse_postfix new_expr rest
      | _ -> (left, tokens)
    in

    match tokens with
    | Increment :: rest ->
      let (expr, remaining) = parse_factor rest in
      (Ast.Assignment {expression1 = expr; expression2 = Ast.Constant 1; compound_operator = Some Ast.PrefixIncrement}, remaining)
    | Decrement :: rest ->
      let (expr, remaining) = parse_factor rest in
      (Ast.Assignment {expression1 = expr; expression2 = Ast.Constant 1; compound_operator = Some Ast.PrefixDecrement}, remaining)
    | (BWComplement :: rest | Negation :: rest | LogicalNot :: rest) ->
      let (unaryop, _) = parse_unary_op tokens in
      let (expr, remaining) = parse_factor rest in
      (Ast.Unary {unary_operator = unaryop; expression = expr}, remaining)
    | _ ->
      let (primary_expr, remaining) = parse_primary tokens in
      parse_postfix primary_expr remaining

  and parse_conditional_middle tokens =
    let remaining = List.tl tokens in
    let (expr, remaining) = parse_expression 0 remaining in
    let remaining = List.tl remaining in
    (expr, remaining)
  
  let parse_optional_expression closing_token tokens =
    match tokens with
    | tok :: rest when tok = closing_token ->
      (None, rest)
    | _ ->
      let (expr, rest) = parse_expression 0 tokens in
      (match rest with
      | tok :: remaining when tok = closing_token ->
        (Some expr, remaining)
      | _ -> raise (ParserError "Expected closing token after expression"))

  let rec parse_statement tokens =
    match tokens with
    | KWReturn :: rest ->
      let (expr, remaining) = parse_expression 0 rest in
      (match remaining with
      | Semicolon :: rest -> (Ast.Return expr, rest)
      | _ -> raise (ParserError "Expected semicolon"))
    | KWIf :: ParenOpen :: rest ->
      let (expr, remaining) = parse_expression 0 rest in
      (match remaining with
      | ParenClose :: rest ->
        let (ifstmt, remaining) = parse_statement rest in
        (match remaining with
        | KWElse :: rest ->
          let (elsestmt, remaining) = parse_statement rest in
          (Ast.If {condition = expr; thenb = ifstmt; elseb = Some elsestmt}, remaining)
        | _ -> (Ast.If {condition = expr; thenb = ifstmt; elseb = None}, remaining)
        )
      | _ -> raise (ParserError "Expected closing parenthesis in if statement"))
    | KWGoto :: Identifier target :: Semicolon :: rest ->
      (Ast.Goto {target = target}, rest)
    | Identifier name :: Colon :: rest ->
      (* Should be invalid in C17 and valid in C23 - add a version flag? *)
      let (_, _) = parse_statement rest in
      (Ast.Label name, rest)
    | Semicolon :: rest -> (Ast.Null, rest)
    | BraceOpen :: _ ->
      let (block, rest) = parse_block tokens in
      (Ast.Compound block, rest)
    | KWBreak :: Semicolon :: rest -> (Ast.Break {label = "DUMMY"}, rest)
    | KWContinue :: Semicolon :: rest -> (Ast.Continue {label = "DUMMY"}, rest)
    | KWWhile :: ParenOpen :: rest ->
      let (expr, remaining) = parse_expression 0 rest in
      (match remaining with
      | ParenClose :: rest ->
        let (stmt, rest) = parse_statement rest in
        (Ast.While {condition = expr; body = stmt; label = "DUMMY"}, rest)
      | _ -> raise (ParserError "Expected closing parenthesis in if statement"))
    | KWDo :: rest ->
      let (stmt, rest) = parse_statement rest in
      (match rest with
      | KWWhile :: ParenOpen :: remaining ->
        let (expr, remaining) = parse_expression 0 remaining in
        (match remaining with
        | ParenClose :: Semicolon :: rest ->
          (Ast.DoWhile {condition = expr; body = stmt; label = "DUMMY"}, rest)
        | _ -> raise (ParserError "Expected closing parenthesis and semicolon after expression"))
      | _ -> raise (ParserError "Expected while and opening parenthesis after do statement"))
    | KWFor :: ParenOpen :: rest ->
      let (init, rest) = parse_for_init rest in
      let (condition, rest) = parse_optional_expression Semicolon rest in
      let (post, rest) = parse_optional_expression ParenClose rest in
      let (body, rest) = parse_statement rest in
      (Ast.For {init = init; condition = condition; post = post; body = body; label = "DUMMY"}, rest)
    | _ ->
      let (expr_opt, rest) = parse_optional_expression Semicolon tokens in
      (match expr_opt with
      | None -> (Ast.Null, rest)
      | Some expr -> (Ast.Expression expr, rest))

  and parse_declaration tokens =
    match tokens with
    | KWInt :: rest ->
      let (name, rest) = parse_id rest in
      (match rest with
      | Assignment :: rest ->
        let (expr, rest) = parse_expression 0 rest in
        (match rest with
        | Semicolon :: rest -> (Ast.Declaration { name; init = Some expr }, rest)
        | _ -> raise (ParserError "Expected semicolon at end of declaration"))
      | Semicolon :: rest -> (Ast.Declaration { name; init = None }, rest)
      | _ -> raise (ParserError "Expected semicolon at end of declaration"))
    | _ -> raise (ParserError "Expected declaration type")
  
  and parse_for_init tokens =
    match tokens with
    | KWInt :: _ ->
      let (decl, rest) = parse_declaration tokens in
      (Ast.InitDecl decl, rest)
    | _ ->
      let (expr_opt, remaining) = parse_optional_expression Semicolon tokens in
      (Ast.InitExp expr_opt, remaining)

  and parse_block_item tokens =
    match tokens with
    | KWInt :: _ ->
      let (decl, rest) = parse_declaration tokens in
      (Ast.D decl, rest)
    | _ ->
      let (stmt, rest) = parse_statement tokens in
      (Ast.S stmt, rest)
  
  and parse_block tokens =
    match tokens with
    | BraceOpen :: rest ->
        let rec aux acc toks =
          match toks with
          | BraceClose :: rest -> (Ast.Block (List.rev acc), rest)
          | [] -> raise (ParserError "Unexpected end of input in block")
          | _ ->
              let (item, rest) = parse_block_item toks in
              aux (item :: acc) rest
        in
        aux [] rest
    | _ -> raise (ParserError "Expected opening brace")


  let parse_function tokens =
    match tokens with
    | KWInt :: rest ->
      let (name, rest) = parse_id rest in
      (match rest with
        | ParenOpen :: KWVoid :: ParenClose :: rest ->
          let (block, rest) = parse_block rest in
          (Ast.Function {name; body = block }, rest)
        | _ -> raise (ParserError "Expected function type")
      )
    | _ -> raise (ParserError "Expected function")

  let parse_program tokens =
    let (function_def, remaining) = parse_function tokens in
    if remaining <> [] then
      raise (ParserError "Unexpected tokens after function definition")
    else
      Ast.Program function_def

end

let parser tokens =
  Private.parse_program tokens