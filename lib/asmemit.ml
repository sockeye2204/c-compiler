open Asm

let show_op = function
  | Register -> "%rax"
  | Imm i -> Printf.sprintf "$%d" i

let emit_instruction out_channel = function
  | Mov (src, dst) ->
    Printf.fprintf out_channel "\tmovq %s, %s\n" (show_op src) (show_op dst)
  | Ret ->
    Printf.fprintf out_channel "\tret\n"

let emit_function out_channel (Function {name; instructions }) =
  Printf.fprintf out_channel {|
    .globl %s
  %s:
  |} ("_" ^ name) ("_" ^ name);
  List.iter (emit_instruction out_channel) instructions

let emit asm_file (Program function_def) =
  let out_channel = open_out asm_file in
  emit_function out_channel function_def;
  close_out out_channel