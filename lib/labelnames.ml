open Tempids
module LabelMap = Map.Make (String)

let rec labelnames_statement mode label_map stmt =
  (match stmt with
  | Ast.Goto {target} ->
    (match mode with
    | "goto" ->
      if LabelMap.mem target label_map then
        (label_map, Ast.Goto {target = LabelMap.find target label_map})
      else
        failwith "Label target not found!"
    | _ -> (label_map, stmt)
    )
  | Ast.Label name ->
    (match mode with
    | "label" ->
      if LabelMap.mem name label_map then
        failwith "Duplicate label name!"
      else
        let unique_name = make_named_temporary name in
        let new_map = LabelMap.add name unique_name label_map in
        (new_map, Ast.Label unique_name)
    | _ -> (label_map, stmt)
    )
  | Ast.If {condition; thenb; elseb} ->
    let new_map, new_thenb = labelnames_statement mode label_map thenb in
    (match elseb with
    | Some stmt ->
      let new_map, new_elseb = labelnames_statement mode label_map stmt in
      (new_map, Ast.If {condition; thenb = new_thenb; elseb = Some new_elseb})
    | None ->
      (new_map, Ast.If {condition; thenb = new_thenb; elseb = None})
    )
  | other -> (label_map, other)
  )

let labelnames_block_item mode label_map block_item =
  (match block_item with
  | Ast.S s ->
    let new_map, labelnames_s = labelnames_statement mode label_map s in
    (new_map, Ast.S labelnames_s)
  | other -> (label_map, other)
  )

  let labelnames_function (Ast.Function {name; body}) =
    let label_map = LabelMap.empty in
    let label_map, labelnames_body =
      List.fold_left_map (labelnames_block_item "label") label_map body
    in
    let _, labelnames_body =
      List.fold_left_map (labelnames_block_item "goto") label_map labelnames_body
    in
    Ast.Function {name; body = labelnames_body}  

let labelnames (Ast.Program function_def) = Ast.Program (labelnames_function function_def)