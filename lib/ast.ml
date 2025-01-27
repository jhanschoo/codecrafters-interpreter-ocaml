open Core

type unop =
  | Minus 
  | Bang

type binop =
  | Equal_equal
  | Bang_equal
  | Less
  | Less_equal
  | Greater
  | Greater_equal
  | Plus
  | Minus
  | Star
  | Slash

type logop =
  | Or
  | And

type lit =
  | Number of Float.t
  | String of String.t
  | Boolean of Bool.t
  | Nil

type expr =
  | Assign of (String.t * expr)
  | Binary of (expr * binop * expr)
  | Call of (expr * expr list)
  | Get of (expr * String.t)
  | Grouping of expr
  | Literal of lit
  | Logical of (expr * logop * expr)
  | Set of (expr * String.t * expr)
  | Super of String.t (* TODO: Token method *)
  | This of String.t (* TODO: Token keyword *)
  | Unary of (unop * expr)
  | Variable of String.t

type stmt =
  | Block of stmt list
  | Class of (String.t * expr option * stmt list)
  | Expression of expr
  | Function of (String.t * String.t list * stmt)
  | If of (expr * stmt * stmt option)
  | Print of expr
  | Return of expr option
  | Var of (String.t * expr option)
  | While of (expr * stmt)

type program = stmt list

let to_string_unop (op : unop) : String.t =
  match op with
  | Minus -> "-"
  | Bang -> "!"

let to_string_binop (op : binop) : String.t =
  match op with
  | Equal_equal -> "=="
  | Bang_equal -> "!="
  | Less -> "<"
  | Less_equal -> "<="
  | Greater -> ">"
  | Greater_equal -> ">="
  | Plus -> "+"
  | Minus -> "-"
  | Star -> "*"
  | Slash -> "/"

let to_string_logop (op : logop) : String.t =
  match op with
  | Or -> "or"
  | And -> "and"

let to_string_lit (l : lit) : String.t =
  match l with
  | Number n -> Util.number_to_string n
  | String s -> s
  | Boolean b -> Bool.to_string b
  | Nil -> "nil"

let rec to_string (e : expr) : String.t =
  match e with
  | Assign (v, e) -> [%string "(assign %{v} %{to_string e})"]
  | Binary (l, op, r) -> [%string "(%{to_string_binop op} %{to_string l} %{to_string r})"]
  | Call (c, args) -> [%string "(call %{to_string c} %{List.to_string ~f:to_string args})"]
  | Get (o, k) -> [%string "(get %{to_string o} %{k})"]
  | Grouping e -> [%string "(group %{to_string e})"]
  | Literal l -> to_string_lit l
  | Logical (l, op, r) -> [%string "(%{to_string l} %{to_string_logop op} %{to_string r})"]
  | Set (o, k, v) -> [%string "(set %{to_string o} %{k} %{to_string v})"]
  | Super m -> [%string "(super %{m})"]
  | This k -> [%string "(this %{k})"]
  | Unary (op, e) -> [%string "(%{to_string_unop op} %{to_string e})"]
  | Variable v -> v

let rec to_string_stmt (s : stmt) : String.t =
  match s with
  | Block stmts -> [%string "(block %{List.to_string ~f:to_string_stmt stmts})"]
  | Class (n, s, stmts) -> [%string "(class %{n} %{List.to_string ~f:to_string (Option.to_list s)} %{List.to_string ~f:to_string_stmt stmts})"]
  | Expression e -> to_string e
  | Function (n, p, b) -> [%string "(fun %{n} %{List.to_string ~f:(fun x -> x) p} %{to_string_stmt b})"]
  | If (c, t, f) -> [%string "(if %{to_string c} %{to_string_stmt t} %{List.to_string ~f:to_string_stmt (Option.to_list f)})"]
  | Print e -> [%string "(print %{to_string e})"]
  | Return e -> [%string "(return %{List.to_string ~f:to_string (Option.to_list e)})"]
  | Var (n, e) -> [%string "(var %{n} %{List.to_string ~f:to_string (Option.to_list e)})"]
  | While (c, b) -> [%string "(while %{to_string c} %{to_string_stmt b})"]
  