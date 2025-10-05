open Asm

let show_op = function
  | Register AX -> "%eax"
  | Register R10 -> "%r10d"
  | Imm i -> Printf.sprintf "$%d" i
  | Stack offset -> Printf.sprintf "%d(%%rbp)" offset
  | Pseudo _ -> failwith "Pseudo registers should have been replaced by this point"
  | _ -> failwith "bleh"

let emit_instruction out_channel = function
  | Mov (src, dst) ->
    Printf.fprintf out_channel "\tmovl %s, %s\n" (show_op src) (show_op dst)
  | Unary {unary_operator = Neg; operand} ->
    Printf.fprintf out_channel "\tnegl %s\n" (show_op operand)
  | Unary {unary_operator = Not; operand} ->
    Printf.fprintf out_channel "\tnotl %s\n" (show_op operand)
  | AllocateStack bytes ->
    Printf.fprintf out_channel "\tsubq $%d, %%rsp\n" bytes
  | Ret ->
    Printf.fprintf out_channel "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n"
  | _ -> failwith "bleh"

let emit_function out_channel (Function {name; instructions }) =
  Printf.fprintf out_channel {|
    .globl %s
  %s:
    pushq %%rbp
    movq %%rsp, %%rbp
  |} ("_" ^ name) ("_" ^ name);
  List.iter (emit_instruction out_channel) instructions

let emit asm_file (Program function_def) =
  let out_channel = open_out asm_file in
  emit_function out_channel function_def;
  close_out out_channel