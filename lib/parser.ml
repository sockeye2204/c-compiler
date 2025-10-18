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
  | Token.Assignment -> Some 1
  | _ -> None

  let parse_id tokens =
    match tokens with
    | Identifier name :: rest -> (name, rest)
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
                 | Token.Assignment ->
                     let remaining2 = List.tl remaining in
                     let (right, remaining3) = parse_expression prec remaining2 in
                     let left = Ast.Assignment { expression1 = left; expression2 = right } in
                     p_e_while left remaining3
                 | _ ->
                     let (operator, remaining2) = parse_binary_op remaining in
                     let (right, remaining3) = parse_expression (prec + 1) remaining2 in
                     let left = Ast.Binary { binary_operator = operator; expression1 = left; expression2 = right } in
                     p_e_while left remaining3)
            | _ -> (left, remaining)
      in
      p_e_while left remaining    

  and parse_factor tokens =
    match tokens with
    | Constant _ :: _ -> parse_int tokens
    | Identifier name :: rest -> (Ast.Var name, rest)
    | (BWComplement :: rest | Negation :: rest | LogicalNot :: rest) ->
      let (unaryop, _) = parse_unary_op tokens in
      let (expr, remaining) = parse_factor rest in
      (Ast.Unary {unary_operator = unaryop; expression = expr}, remaining )
    | ParenOpen :: rest ->
      let (expr, remaining) = parse_expression 0 rest in
      (match remaining with
      | ParenClose :: rest -> (expr, rest)
      | _ -> raise (ParserError "Expected closing brace"))
    | _ -> raise (ParserError "Expected factor")

  let parse_statement tokens =
    match tokens with
    | KWReturn :: rest ->
      let (expr, remaining) = parse_expression 0 rest in
      (match remaining with
      | Semicolon :: rest -> (Ast.Return expr, rest)
      | _ -> raise (ParserError "Expected semicolon"))
    | Semicolon :: rest -> (Ast.Null, rest)
    | _ ->
      let (expr, remaining) = parse_expression 0 tokens in
      (match remaining with
      | Semicolon :: rest -> (Ast.Expression expr, rest)
      | _ -> raise (ParserError "Expected semicolon"))

  let parse_declaration tokens =
    match tokens with
    | KWInt :: rest ->
      let (name, rest) = parse_id rest in
      (match rest with
      | Assignment :: rest ->
        let (expr, rest) = parse_expression 0 rest in
        (match rest with
        | Semicolon :: rest -> (Ast.Declaration { name; init = Some expr }, rest)
        | _ -> raise (ParserError "Expected semicolon at end of declaration"))
      | Semicolon :: rest -> (Ast.Declaration { name; init = None}, rest)
      | _ -> raise (ParserError "Expected semicolon at end of declaration"))
    | _ -> raise (ParserError "Expected declaration type")

  let parse_block_item tokens =
    match tokens with
    | KWInt :: _ ->
      let (decl, rest) = parse_declaration tokens in
      (Ast.D decl, rest)
    | _ -> 
      let (stmt, rest) = parse_statement tokens in
      (Ast.S stmt, rest)

  let parse_function tokens =
    match tokens with
    | KWInt :: rest ->
      let (name, rest) = parse_id rest in
      (match rest with 
        | ParenOpen :: KWVoid :: ParenClose :: BraceOpen :: rest ->
          let tokens_ref = ref rest in
          let block_items = ref [] in

          (* Consume block items until we reach a closing brace *)
          while match !tokens_ref with
            | BraceClose :: _ -> false
            | [] -> raise (ParserError "Unexpected end of input in function body")
            | _ -> true
          do
            let (block_item, remaining) = parse_block_item !tokens_ref in
            block_items := !block_items @ [block_item];
            tokens_ref := remaining
          done;

          (match !tokens_ref with
          | BraceClose :: rest -> (Ast.Function { name; body = !block_items }, rest)
          | _ -> raise (ParserError "Expected closing brace"))
        | _ -> raise (ParserError "Expected function body")) 
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
