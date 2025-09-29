val fixup_instruction : Asm.instruction -> Asm.instruction list
val fixup_function : int -> Asm.function_def -> Asm.function_def
val fixup_program : int -> Asm.t -> Asm.t
