open Tempids
module StringMap = Map.Make (String)

let rec resolve_exp var_map exp =
  match exp with
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
    if StringMap.mem v var_map then
      Ast.Var (StringMap.find v var_map)
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
  if StringMap.mem name var_map then failwith "Duplicate variable declaration!"
  else
    let unique_name = make_named_temporary name in
    let new_map = StringMap.add name unique_name var_map in
    let resolved_init = Option.map (resolve_exp new_map) init in
    (new_map, Ast.Declaration {name=unique_name; init=resolved_init})

let resolve_statement var_map stmt =
  match stmt with
  | Ast.Return exp -> Ast.Return (resolve_exp var_map exp)
  | Ast.Expression exp -> Ast.Expression (resolve_exp var_map exp)
  | Ast.Null -> Ast.Null

let resolve_block_item var_map block_item =
  match block_item with
  | Ast.S s ->
    let resolved_s = resolve_statement var_map s in
    (var_map, Ast.S resolved_s)
  | Ast.D d ->
    let new_map, resolved_d = resolve_declaration var_map d in
    (new_map, Ast.D resolved_d)

let resolve_function (Ast.Function {name; body}) =
  let var_map = StringMap.empty in
  let _final_map, resolved_body =
    List.fold_left_map resolve_block_item var_map body
  in
  Ast.Function {name; body = resolved_body}

let resolve (Ast.Program function_def) = Ast.Program (resolve_function function_def)