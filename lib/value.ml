type t =
  | Number of float
  | String of string
  | Boolean of bool
  | Nil
  | NativeCallable of (int * (t list -> t))
  | Callable of callable

and callable =
  { arity : int
  ; identifier : string
  ; params : String.t list
  ; env : t Environment.t
  ; body : Ast.stmt
  }

let to_string (v : t) : string =
  match v with
  (* Note: test requirements do not accept format of Util.number_to_string *)
  | Number n -> Util.number_value_to_string n
  | String s -> s
  | Boolean true -> "true"
  | Boolean false -> "false"
  | Nil -> "nil"
  | NativeCallable _ -> "<native fn>"
  | Callable { identifier; _ } -> Printf.sprintf "<fn %s>" identifier
;;

let is_truthy (v : t) : bool =
  match v with
  | Boolean b -> b
  | Nil -> false
  | _ -> true
;;
