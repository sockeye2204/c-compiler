open Token
exception ParserError of string

module Private = struct
  let precedence = function
  | Token.Multiplication | Token.Division | Token.Remainder -> Some 30
  | Token.Addition | Token.Negation -> Some 20
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
    | BWComplement :: rest -> (Ast.Complement, rest)
    | Negation :: rest -> (Ast.Negate, rest)
    | _ -> raise (ParserError "Expected unary operator")
  
  let parse_binary_op tokens =
    match tokens with
    | Multiplication :: rest -> (Ast.Multiply, rest)
    | Division :: rest -> (Ast.Divide, rest)
    | Remainder :: rest -> (Ast.Modulo, rest)
    | Addition :: rest -> (Ast.Add, rest)
    | Negation :: rest -> (Ast.Subtract, rest)
    | _ -> raise (ParserError "Expected binary operator")

  let rec parse_expression min_prec tokens =
    let (left, remaining) = parse_factor tokens in
    let rec p_e_while left remaining =
      match remaining with
      | [] -> (left, remaining)
      | next :: _ ->
          match precedence next with
          | Some prec when prec >= min_prec ->
              let (operator, remaining2) = parse_binary_op remaining in
              let (right, remaining3) = parse_expression (prec+1) remaining2 in
              let left = Ast.Binary{binary_operator=operator; expression1=left; expression2=right} in
              p_e_while left remaining3
          | _ -> (left, remaining)
    in
    p_e_while left remaining

  and parse_factor tokens =
    match tokens with
    | Constant _ :: _ -> parse_int tokens
    | (BWComplement :: rest | Negation :: rest) ->
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
    | _ -> raise (ParserError "Expected return statement")

  let parse_function tokens =
    match tokens with
    | KWInt :: rest ->
      let (name, rest) = parse_id rest in
      (match rest with 
        | ParenOpen :: KWVoid :: ParenClose :: BraceOpen :: rest ->
          let (statement, remaining) = parse_statement rest in
          (match remaining with
          | BraceClose :: rest -> (Ast.Function { name; body = statement }, rest)
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
