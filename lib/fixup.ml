open Asm

let fixup_instruction = function
  | Mov ((Stack _ as src), (Stack _ as dst)) ->
      [ Mov (src, Register R10); Mov (Register R10, dst) ]
  | Idiv (Imm operand) -> [ Mov (Imm operand, Register R10); Idiv (Register R10)]
  | Binary
    { binary_operator=(Add|Sub) as binary_operator; operand1=Stack _ as operand1; operand2=Stack _ as operand2} ->
      [ Mov (operand1, Register R10); Binary {binary_operator; operand1=(Register R10); operand2}]
  | Binary
    { binary_operator=Mult; operand1; operand2=Stack _ as operand2} ->
      [
        Mov (operand2, Register R11);
        Binary { binary_operator=Mult; operand1; operand2=(Register R11)};
        Mov (Register R11, operand2)
      ]
  | other -> [ other ]

let fixup_function last_stack_slot (Function { name; instructions }) =
  Function
    {
      name;
      instructions =
        AllocateStack (-last_stack_slot)
        :: List.concat_map fixup_instruction instructions;
    }

let fixup_program last_stack_slot (Program fn_def) =
  Program (fixup_function last_stack_slot fn_def)
