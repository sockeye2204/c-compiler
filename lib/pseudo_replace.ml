module StringMap = Map.Make (String)

type state = {
  off: int;
  offmap: int StringMap.t;
}

let operand_replace state = function
  | Asm.Pseudo pseudo ->
    (match StringMap.find_opt pseudo state.offmap with
     | Some off -> (state, Asm.Stack off)
     | None ->
       let new_off = state.off - 4 in
       let new_state = {
         off = new_off;
         offmap = StringMap.add pseudo new_off state.offmap;
       } in
       (new_state, Asm.Stack new_off))
  | other -> (state, other)

let instruction_replace state = function
  | Asm.Mov (src, dst) ->
    let state1, new_src = operand_replace state src in
    let state2, new_dst = operand_replace state1 dst in
    (state2, Asm.Mov (new_src, new_dst))
  | Asm.Unary {unary_operator; operand} ->
    let state1, new_operand = operand_replace state operand in
    (state1, Asm.Unary {unary_operator; operand = new_operand})
  | Asm.Binary {binary_operator; operand1; operand2} ->
    let state1, new_operand1 = operand_replace state operand1 in
    let state2, new_operand2 = operand_replace state1 operand2 in
    (state2, Asm.Binary {binary_operator; operand1 = new_operand1; operand2 = new_operand2})
  | Asm.Idiv operand ->
    let state1, new_operand = operand_replace state operand in
    (state1, Asm.Idiv new_operand)
  | Asm.Cmp {operand1; operand2} ->
    let state1, new_operand1 = operand_replace state operand1 in
    let state2, new_operand2 = operand_replace state1 operand2 in
    (state2, Asm.Cmp {operand1 = new_operand1; operand2 = new_operand2})
  | Asm.SetCC {cond_code; operand} ->
    let state1, new_operand = operand_replace state operand in
    (state1, Asm.SetCC {cond_code; operand = new_operand})
  | (Asm.Ret | Asm.Cdq | Asm.Jmp _ | Asm.JmpCC _ | Asm.Label _) as other -> (state, other)
  | Asm.AllocateStack _ -> failwith "AllocateStack unexpected in instruction_replace"

let function_replace (Asm.Function {name; instructions}) =
  let state0 = {off = 0; offmap = StringMap.empty} in
  let statef, fixed_instructions =
    List.fold_left_map instruction_replace state0 instructions in
  (Asm.Function {name; instructions = fixed_instructions}, statef.off)

let pseudo_replace (Asm.Program program_def) =
  let fixed_progdef, last_off = function_replace program_def in
  (Asm.Program fixed_progdef, last_off)
