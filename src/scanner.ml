open Core

let tokenize (file_contents : string) : Token.t list =
  let lineref = ref 0
  and posref = ref 0 in
  let f (acc : Token.t list) (c : char) : Token.t list =
    posref := !posref + 1;
    let line = !lineref
    and pos = !posref in
    let single_char_tmpl (tt : Token_type.t) : Token.t list =
      { tt; lexeme = String.of_char c; line; pos } :: acc
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
    | _ -> acc
  in
  let rev_res =
    Token.{ tt = Eof; lexeme = ""; line = !lineref; pos = !posref + 1 }
    :: String.fold file_contents ~init:[] ~f
  in
  List.rev rev_res
;;
