open C_compiler.Lexer
open C_compiler.Token
open C_compiler.Parser
open C_compiler.Asmgen

let print_token = function
  | Identifier s -> Printf.printf "Identifier(\"%s\")\n" s
  | Constant n -> Printf.printf "Constant(%d)\n" n
  | KWInt -> print_endline "KWInt"
  | KWVoid -> print_endline "KWVoid"
  | KWReturn -> print_endline "KWReturn"
  | ParenOpen -> print_endline "ParenOpen"
  | ParenClose -> print_endline "ParenClose"
  | BraceOpen -> print_endline "BraceOpen"
  | BraceClose -> print_endline "BraceClose"
  | Semicolon -> print_endline "Semicolon"

let lex_input input =
  try
    let tokens = lexer input in
    Printf.printf "Lexed %d tokens:\n" (List.length tokens);
    List.iter print_token tokens
  with
  | LexerError msg -> Printf.printf "Lexer error: %s\n" msg

let lex_file filename =
  if Sys.file_exists filename then (
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    try
      let _tokens = lexer content in
      exit 0
    with
    | LexerError _ -> 
      exit 1
  ) else (
    Printf.printf "File not found: %s\n" filename;
    exit 1
  )

let parse_file filename =
  if Sys.file_exists filename then (
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    try
      let tokens = lexer content in
      let _ast = parser tokens in
      exit 0
    with
    | LexerError _ | ParserError _ -> 
      exit 1
  ) else (
    Printf.printf "File not found: %s\n" filename;
    exit 1
  )

let asmgen_file filename =
  if Sys.file_exists filename then (
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    try
      let tokens = lexer content in
      let ast = parser tokens in
      let _asmt = asmgen ast in
      exit 0
    with
    | LexerError _ | ParserError _ ->
      exit 1
  ) else (
    Printf.printf "File not found: %s\n" filename;
    exit 1
  )

let () =
  match Array.to_list Sys.argv with
  | [_; "--lex"; filename] ->
    lex_file filename
  | [_; "--parse"; filename] ->
    parse_file filename
  | [_; "--codegen"; filename] ->
    asmgen_file filename
  | [_; filename] ->
    if Sys.file_exists filename then (
      let ic = open_in filename in
      let content = really_input_string ic (in_channel_length ic) in
      close_in ic;
      Printf.printf "Lexing file: %s\n" filename;
      lex_input content
    ) else (
      Printf.printf "File not found: %s\n" filename;
      exit 1
    )
  | _ ->
    print_endline "Usage: main [--lex|--parse] <filename>";
    print_endline "  --lex: Test framework mode (just lex and exit)";
    print_endline "  --parse: Test framework mode (lex + parse and exit)";
    print_endline "  --codegen: Test framework mode (lex + parse + asmgen and exit)";
    print_endline "  Without flags: Interactive mode (print tokens)";
    exit 1
