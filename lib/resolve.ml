open Tempids
module VarMap = Map.Make (String)

type map_entry = { unique_name: string; from_current_block: bool }

let wash_var_map map =
  VarMap.map (fun slot -> { slot with from_current_block = false }) map

let rec resolve_exp var_map exp =
  match exp with
  | Ast.Conditional { condition; expression1; expression2 } ->
    Ast.Conditional {
      condition = resolve_exp var_map condition;
      expression1 = resolve_exp var_map expression1;
      expression2 = resolve_exp var_map expression2;
    }
  | Ast.Assignment { expression1; expression2; compound_operator } ->
    (match expression1 with
     | Ast.Var _ ->
       Ast.Assignment {
         expression1 = resolve_exp var_map expression1;
         expression2 = resolve_exp var_map expression2;
         compound_operator = compound_operator;
       }
     | _ ->
       failwith "Invalid lvalue! Must be Var AST node")
  | Ast.Var v ->
    if VarMap.mem v var_map then
      Ast.Var (VarMap.find v var_map).unique_name
    else
      failwith "Undeclared variable!"
  | Ast.Unary {unary_operator; expression} ->
    Ast.Unary {
      unary_operator;
      expression = resolve_exp var_map expression
    }
  | Ast.Binary {binary_operator; expression1; expression2} ->
    Ast.Binary {
      binary_operator;
      expression1 = resolve_exp var_map expression1;
      expression2 = resolve_exp var_map expression2;
    }
  | Ast.Constant _ as c -> c

let resolve_declaration var_map (Ast.Declaration {name; init}) =
  (match VarMap.find_opt name var_map with
  | Some { from_current_block = true; _ } ->
    failwith "Duplicate variable declaration in block!"
  | _ ->
    let unique_name = make_named_temporary name in
    let new_map = VarMap.add name {unique_name; from_current_block = true} var_map in
    let resolved_init = Option.map (resolve_exp new_map) init in
    (new_map, Ast.Declaration {name=unique_name; init=resolved_init}))

let rec resolve_statement var_map stmt =
  match stmt with
  | Ast.Return exp -> Ast.Return (resolve_exp var_map exp)
  | Ast.Expression exp -> Ast.Expression (resolve_exp var_map exp)
  | Ast.If { condition; thenb; elseb } ->
    Ast.If {
      condition = resolve_exp var_map condition;
      thenb = resolve_statement var_map thenb;
      elseb = (match elseb with
      | Some stmt -> Some (resolve_statement var_map stmt)
      | None -> None);
    }
  | Ast.Goto target -> Ast.Goto target
  | Ast.Label name -> Ast.Label name
  | Ast.Compound block ->
    let washed_map = wash_var_map var_map in
    let _, resolved_block = resolve_block washed_map block in
    Ast.Compound resolved_block  
  | Ast.Null -> Ast.Null
  | _ -> failwith "todo"

and resolve_block_item var_map block_item =
  match block_item with
  | Ast.S s ->
    let resolved_s = resolve_statement var_map s in
    (var_map, Ast.S resolved_s)
  | Ast.D d ->
    let new_map, resolved_d = resolve_declaration var_map d in
    (new_map, Ast.D resolved_d)

and resolve_block var_map (Ast.Block block_items) =
  let new_map, resolved_block_items =
    List.fold_left_map resolve_block_item var_map block_items
  in
  (new_map, Ast.Block resolved_block_items)

let resolve_function (Ast.Function {name; body}) =
  let var_map = VarMap.empty in
  let (_, resolved_body) = resolve_block var_map body in 
  Ast.Function {name; body = resolved_body}

let resolve (Ast.Program function_def) = Ast.Program (resolve_function function_def)