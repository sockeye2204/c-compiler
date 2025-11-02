open Tempids

let annotate current_label stmt =
  match stmt with
  | Ast.Break _ ->
    (Ast.Break {label=current_label})
  | Ast.Continue _ ->
    (Ast.Continue {label=current_label})
  | Ast.While {condition; body; _} ->
    (Ast.While {
      condition;
      body;
      label = current_label;
    })
  | Ast.DoWhile {body; condition; _} ->
    (Ast.DoWhile {
      body;
      condition;
      label = current_label;
    })
  | Ast.For {init; condition; post; body; _} ->
    (Ast.For {
      init;
      condition;
      post;
      body;
      label = current_label;
    })
  | _ -> stmt

let rec ll_statement current_label stmt =
  match stmt with
  | Ast.Break _
  | Ast.Continue _ ->
    if current_label = "" then
      failwith "Break statement outside loop"
    else
      let annotated_stmt = annotate current_label stmt in
      (annotated_stmt)
  | Ast.While {condition; body; label} ->
    let new_label = make_label "loop" in
    let lled_body = ll_statement new_label body in
    let lled_stmt = Ast.While {condition; body=lled_body; label} in
    let annotated_stmt = annotate new_label lled_stmt in
    (annotated_stmt)
  | Ast.DoWhile {body; condition; label} ->
    let new_label = make_label "loop" in
    let lled_body = ll_statement new_label body in
    let lled_stmt = Ast.DoWhile {body=lled_body; condition; label} in
    let annotated_stmt = annotate new_label lled_stmt in
    (annotated_stmt)
  | Ast.For {init; condition; post; body; label} ->
    let new_label = make_label "loop" in
    let lled_body = ll_statement new_label body in
    let lled_stmt = Ast.For {init; condition; post; body=lled_body; label} in
    let annotated_stmt = annotate new_label lled_stmt in
    (annotated_stmt)
  | Ast.Compound block ->
    let lled_block = ll_block current_label block in
    (Ast.Compound lled_block)
  | Ast.If {condition; thenb; elseb} ->
    let lled_thenb = ll_statement current_label thenb in
    let lled_elseb =
      match elseb with
      | Some s -> Some (ll_statement current_label s)
      | None -> None
    in
    Ast.If {condition; thenb=lled_thenb; elseb=lled_elseb}  
  | _ -> stmt

and ll_block_item current_label block_item =
  match block_item with
  | Ast.S s ->
    let lled_block_item = ll_statement current_label s in
    (Ast.S lled_block_item)
  | Ast.D d -> Ast.D d

and ll_block current_label (Ast.Block block_items) =
  let lled_block_items =
    List.map (ll_block_item current_label) block_items
  in
  Ast.Block lled_block_items  

let ll_function (Ast.Function {name; body}) =
  let current_label = "" in
  let lled_body = ll_block current_label body in
  Ast.Function {name; body=lled_body}

let loop_label (Ast.Program function_def) = Ast.Program (ll_function function_def)