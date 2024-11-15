open Core

type t =
  | Left_Paren
  | Right_Paren
  | Left_Brace
  | Right_Brace
  | Comma
  | Dot
  | Minus
  | Plus
  | Semicolon
  | Slash
  | Star
  | Bang
  | Bang_Equal
  | Equal
  | Equal_Equal
  | Greater
  | Greater_Equal
  | Less
  | Less_Equal
  | Identifier of String.t [@nested "IDENTIFIER."]
  | String of String.t [@nested "STRING."]
  | Number of Float.t [@nested "NUMBER."]
  | And
  | Class
  | Else
  | False
  | Fun
  | For
  | If
  | Nil
  | Or
  | Print
  | Return
  | Super
  | This
  | True
  | Var
  | While
  | Eof
  [@@deriving to_string ~capitalize:"SCREAMING_SNAKE_CASE", sexp]