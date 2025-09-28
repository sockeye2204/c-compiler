open Token
exception ParserError of string

module Private = struct
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

  let rec parse_expression tokens =
    match tokens with
    | Constant _ :: _ -> parse_int tokens
    | (BWComplement :: rest | Negation :: rest) ->
      let (unaryop, _) = parse_unary_op tokens in
      let (expr, remaining) = parse_expression rest in
      (Ast.Unary {unary_operator = unaryop; expression = expr}, remaining )
    | ParenOpen :: rest ->
      let (expr, remaining) = parse_expression rest in
      (match remaining with
      | ParenClose :: rest -> (expr, rest)
      | _ -> raise (ParserError "Expected closing brace"))
    | _ -> raise (ParserError "Expected expression")

  let parse_statement tokens =
    match tokens with
    | KWReturn :: rest ->
      let (expr, remaining) = parse_expression rest in
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
