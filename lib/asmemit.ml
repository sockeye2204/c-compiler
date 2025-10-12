open Asm

let local_label label =
  "L" ^ label

let show_cond_code = function
  | E -> "e"
  | NE -> "ne"
  | G -> "g"
  | GE -> "ge"
  | L -> "l"
  | LE -> "le"

let show_op = function
  | Register AX -> "%eax"
  | Register DX -> "%edx"
  | Register R10 -> "%r10d"
  | Register R11 -> "%r11d"
  | Imm i -> Printf.sprintf "$%d" i
  | Stack offset -> Printf.sprintf "%d(%%rbp)" offset
  | Pseudo _ -> failwith "Pseudo registers should have been replaced by this point"

let show_byte_op = function
  | Register AX -> "%al"
  | Register DX -> "%dl"
  | Register R10 -> "%r10b"
  | Register R11 -> "%r11b"
  | other -> show_op other

let emit_instruction out_channel = function
  | Mov (src, dst) ->
    Printf.fprintf out_channel "\tmovl %s, %s\n" (show_op src) (show_op dst)
  | Cmp {operand1; operand2} ->
    Printf.fprintf out_channel "\tcmpl %s, %s\n" (show_op operand1) (show_op operand2)
  | Unary {unary_operator = Neg; operand} ->
    Printf.fprintf out_channel "\tnegl %s\n" (show_op operand)
  | Unary {unary_operator = Not; operand} ->
    Printf.fprintf out_channel "\tnotl %s\n" (show_op operand)
  | Binary {binary_operator = Add; operand1; operand2} ->
    Printf.fprintf out_channel "\taddl %s, %s\n" (show_op operand1) (show_op operand2)
  | Binary {binary_operator = Sub; operand1; operand2} ->
    Printf.fprintf out_channel "\tsubl %s, %s\n" (show_op operand1) (show_op operand2)
  | Binary {binary_operator = Mult; operand1; operand2} ->
    Printf.fprintf out_channel "\timull %s, %s\n" (show_op operand1) (show_op operand2)
  | Jmp label ->
    Printf.fprintf out_channel "\tjmp %s\n" (local_label label)
  | JmpCC {cond_code; identifier} ->
    Printf.fprintf out_channel "\tj%s %s\n" (show_cond_code cond_code) (local_label identifier)
  | SetCC {cond_code; operand} ->
    Printf.fprintf out_channel "\tset%s %s\n" (show_cond_code cond_code) (show_byte_op operand)
  | Label label ->
    Printf.fprintf out_channel "\t%s:\n" (local_label label)
  | Idiv operand ->
    Printf.fprintf out_channel "\tidivl %s\n" (show_op operand)
  | Cdq ->
    Printf.fprintf out_channel "\tcdq\n"
  | AllocateStack bytes ->
    Printf.fprintf out_channel "\tsubq $%d, %%rsp\n" bytes
  | Ret ->
    Printf.fprintf out_channel "\tmovq %%rbp, %%rsp\n\tpopq %%rbp\n\tret\n"

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