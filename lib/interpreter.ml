open Core

let evaluate_lit (lit : Ast.lit) : Value.t =
  match lit with
  | Ast.Number n -> Value.Number n
  | Ast.String s -> Value.String s
  | Ast.Boolean b -> Value.Boolean b
  | Ast.Nil -> Value.Nil
;;

let rec evaluate_expr (env : Environment.t) (expr : Ast.expr) : Value.t =
  match expr with
  | Ast.Logical (e1, op, e2) ->
    let v1 = evaluate_expr env e1 in
    (match op with
     | Ast.Or -> if Value.is_truthy v1 then v1 else evaluate_expr env e2
     | Ast.And -> if not (Value.is_truthy v1) then v1 else evaluate_expr env e2)
  | Ast.Binary (e1, op, e2) ->
    let v1 = evaluate_expr env e1 in
    let v2 = evaluate_expr env e2 in
    (match op, v1, v2 with
     | Ast.Bang_equal, Value.Number n1, Value.Number n2 -> Value.Boolean Float.(n1 <> n2)
     | Ast.Bang_equal, Value.String s1, Value.String s2 -> Value.Boolean String.(s1 <> s2)
     | Ast.Bang_equal, Value.Boolean b1, Value.Boolean b2 -> Value.Boolean Bool.(b1 <> b2)
     | Ast.Bang_equal, Value.Nil, Value.Nil -> Value.Boolean false
     | Ast.Bang_equal, _, _ -> Value.Boolean true
     | Ast.Equal_equal, Value.Number n1, Value.Number n2 -> Value.Boolean Float.(n1 = n2)
     | Ast.Equal_equal, Value.String s1, Value.String s2 -> Value.Boolean String.(s1 = s2)
     | Ast.Equal_equal, Value.Boolean b1, Value.Boolean b2 -> Value.Boolean Bool.(b1 = b2)
     | Ast.Equal_equal, Value.Nil, Value.Nil -> Value.Boolean true
     | Ast.Equal_equal, _, _ -> Value.Boolean false
     | Ast.Greater, Value.Number n1, Value.Number n2 -> Value.Boolean Float.(n1 > n2)
     | Ast.Greater_equal, Value.Number n1, Value.Number n2 ->
       Value.Boolean Float.(n1 >= n2)
     | Ast.Less, Value.Number n1, Value.Number n2 -> Value.Boolean Float.(n1 < n2)
     | Ast.Less_equal, Value.Number n1, Value.Number n2 -> Value.Boolean Float.(n1 <= n2)
     | Ast.Minus, Value.Number s1, Value.Number s2 -> Value.Number (s1 -. s2)
     | Ast.Plus, Value.Number s1, Value.Number s2 -> Value.Number (s1 +. s2)
     | Ast.Plus, Value.String s1, Value.String s2 -> Value.String (s1 ^ s2)
     | Ast.Slash, Value.Number n1, Value.Number n2 -> Value.Number (n1 /. n2)
     | Ast.Star, Value.Number n1, Value.Number n2 -> Value.Number (n1 *. n2)
     | Ast.Plus, _, _ ->
       Printf.eprintf "Operands must be two numbers or two strings.\n";
       exit 70
     | Ast.Greater, _, _
     | Ast.Greater_equal, _, _
     | Ast.Less, _, _
     | Ast.Less_equal, _, _
     | Ast.Minus, _, _
     | Ast.Slash, _, _
     | Ast.Star, _, _ ->
       Printf.eprintf "Operands must be numbers.\n";
       exit 70)
  | Ast.Literal lit -> evaluate_lit lit
  | Ast.Grouping e -> evaluate_expr env e
  | Ast.Unary (op, e) ->
    let v = evaluate_expr env e in
    (match op, v with
     | Ast.Bang, v when Value.is_truthy v -> Value.Boolean false
     | Ast.Bang, _ -> Value.Boolean true
     | Ast.Minus, Value.Number n -> Value.Number (-.n)
     | Ast.Minus, _ ->
       Printf.eprintf "Operand must be a number\n";
       exit 70)
  | Ast.Variable v ->
    (match Environment.get env v with
     | Some v -> v
     | None ->
       Printf.eprintf "Undefined variable '%s'.\n" v;
       exit 70)
  | Ast.Assign (var, e) ->
    let v = evaluate_expr env e in
    Environment.set env var v;
    v
  | Ast.Call (callee, args) ->
    let f = evaluate_expr env callee in
    let args = List.map args ~f:(evaluate_expr env) in
    let guardarity arity =
      if List.length args <> arity
      then (
        Printf.eprintf "Expected %d arguments but got %d.\n" arity (List.length args);
        exit 70)
    in
    (match f with
     | Value.NativeCallable (arity, f) ->
       guardarity arity;
       f args
     | _ ->
       Printf.eprintf "Can only call functions and classes.\n";
       exit 70)
  | _ ->
    Printf.eprintf "Not implemented yet.\n";
    exit 70
;;

let evaluate = evaluate_expr (Environment.create None)

let rec evaluate_prog (env : Environment.t) (prog : Ast.program) : Unit.t =
  let f (stmt : Ast.stmt) : Unit.t =
    match stmt with
    | Ast.Var (name, Some expr) -> Environment.define env name (evaluate_expr env expr)
    | Ast.Var (name, None) -> Environment.define env name Value.Nil
    | Ast.Print e -> Printf.printf "%s\n" (Value.to_string (evaluate_expr env e))
    | Ast.Expression e -> ignore (evaluate_expr env e : Value.t)
    | Ast.Block stmts ->
      let new_env = Environment.create (Some env) in
      evaluate_prog new_env stmts
    | Ast.If (cond, then_branch, else_branch) ->
      if Value.is_truthy (evaluate_expr env cond)
      then evaluate_prog env [ then_branch ]
      else (
        match else_branch with
        | Some stmt -> evaluate_prog env [ stmt ]
        | None -> ())
    | Ast.While (cond, body) ->
      while Value.is_truthy (evaluate_expr env cond) do
        evaluate_prog env [ body ]
      done
    | _ ->
      Printf.eprintf "Not implemented yet.\n";
      exit 70
  in
  List.iter prog ~f
;;

let execute = evaluate_prog (Environment.create None)
