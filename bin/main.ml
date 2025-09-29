open C_compiler.Lexer
open C_compiler.Parser
open C_compiler.Tacer
open C_compiler.Asmgen
open C_compiler.Pseudo_replace
open C_compiler.Fixup
open C_compiler.Asmemit

let read_file filename =
  if Sys.file_exists filename then
    let ic = open_in filename in
    let content = really_input_string ic (in_channel_length ic) in
    close_in ic;
    Some content
  else (
    Printf.eprintf "File not found: %s\n" filename;
    None
  )

let run_stage filename f =
  match read_file filename with
  | None -> exit 1
  | Some content ->
    try f content; exit 0
    with LexerError _ | ParserError _ -> exit 1

let lex_file filename =
  run_stage filename (fun content -> ignore (lexer content))

let parse_file filename =
  run_stage filename (fun content ->
    lexer content |> parser |> ignore
  )

let tac_file filename =
  run_stage filename (fun content ->
    lexer content |> parser |> tacer |> ignore
  )

let codegen_file filename =
  run_stage filename (fun content ->
    let asm_ast = lexer content |> parser |> tacer |> asmgen in
    let asm_ast', _ = pseudo_replace asm_ast in
    let asm_ast'' = fixup_program 0 asm_ast' in
    ignore asm_ast''
  )

let generate_asm filename =
  run_stage filename (fun content ->
    let asm_ast = lexer content |> parser |> tacer |> asmgen in
    let asm_ast', last_off = pseudo_replace asm_ast in
    let asm_ast'' = fixup_program last_off asm_ast' in
    let base = Filename.chop_extension filename in
    let asm_file = base ^ ".s" in
    emit asm_file asm_ast'';
    Printf.printf "Generated assembly file: %s\n" asm_file
  )

let compile_to_executable filename =
  run_stage filename (fun content ->
    let asm_ast = lexer content |> parser |> tacer |> asmgen in
    let asm_ast', last_off = pseudo_replace asm_ast in
    let asm_ast'' = fixup_program last_off asm_ast' in
    let base = Filename.chop_extension filename in
    let asm_file = base ^ ".s" in
    let exe_file = base in

    emit asm_file asm_ast'';
    let gcc_cmd = Printf.sprintf "gcc -m64 -arch x86_64 %s -o %s" asm_file exe_file in
    if Sys.command gcc_cmd = 0 then Sys.remove asm_file else exit 1
  )

let () =
  match Array.to_list Sys.argv with
  | [_; "--lex"; filename]     -> lex_file filename
  | [_; "--parse"; filename]   -> parse_file filename
  | [_; "--tacky"; filename]   -> tac_file filename
  | [_; "--codegen"; filename] -> codegen_file filename
  | [_; "--asm"; filename]     -> generate_asm filename
  | [_; filename]              -> compile_to_executable filename
  | _ ->
    print_endline "Usage: main [--lex|--parse|--codegen|--asm] <filename>";
    print_endline "  --lex:     Lex only";
    print_endline "  --parse:   Lex + parse";
    print_endline "  --tacky:   Lex + parse + tac";
    print_endline "  --codegen: Lex + parse + tac + codegen";
    print_endline "  --asm:     Generate assembly file (.s)";
    print_endline "  (default): Full compilation to executable";
    exit 1
