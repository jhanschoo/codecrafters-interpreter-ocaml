open Core

type error = string
type result = Token.t list * error list

let tokenize (file_contents : string) : result =
  let lineref = ref 1
  and posref = ref 0 in
  let f ((acc, errs) : result) (c : char) : result =
    posref := !posref + 1;
    let line = !lineref
    and pos = !posref in
    let single_char_tmpl (tt : Token_type.t) : result =
      { tt; lexeme = String.of_char c; line; pos } :: acc, errs
    in
    match c with
    | '(' -> single_char_tmpl Left_Paren
    | ')' -> single_char_tmpl Right_Paren
    | '{' -> single_char_tmpl Left_Brace
    | '}' -> single_char_tmpl Right_Brace
    | ',' -> single_char_tmpl Comma
    | '.' -> single_char_tmpl Dot
    | '-' -> single_char_tmpl Minus
    | '+' -> single_char_tmpl Plus
    | ';' -> single_char_tmpl Semicolon
    | '*' -> single_char_tmpl Star
    | '/' -> single_char_tmpl Slash
    | _ ->
      let e = Printf.sprintf "[line %i] Error: Unexpected character: %c" line c in
      Printf.eprintf "%s\n" e;
      acc, e :: errs
  in
  let acc, errs = String.fold file_contents ~init:([], []) ~f in
  ( Token.{ tt = Eof; lexeme = ""; line = !lineref; pos = !posref + 1 } :: acc |> List.rev
  , errs )
;;
