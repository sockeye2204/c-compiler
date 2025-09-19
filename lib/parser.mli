exception ParserError of string
val parser: Token.t list -> Ast.t