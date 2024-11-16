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

type lit =
  | Number of Float.t
  | String of String.t
  | Boolean of Bool.t
  | Nil

type expr =
  | Binary of (expr * binop * expr)
  | Grouping of expr
  | Literal of lit
  | Unary of (unop * expr)

type t = expr

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

let to_string_lit (l : lit) : String.t =
  match l with
  | Number n -> Util.number_to_string n
  | String s -> s
  | Boolean b -> Bool.to_string b
  | Nil -> "nil"

let rec to_string (e : expr) : String.t =
  match e with
  | Binary (l, op, r) -> [%string "(%{to_string l} %{to_string_binop op} %{to_string r})"]
  | Grouping e -> [%string "(group %{to_string e})"]
  | Literal l -> to_string_lit l
  | Unary (op, e) -> [%string "(%{to_string_unop op} %{to_string e})"]