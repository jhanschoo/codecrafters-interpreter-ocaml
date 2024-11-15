open Core

let tokenize (file_contents : string) : Token.t list =
  let lineref = ref 0
  and posref = ref 0 in
  let f (acc : Token.t list) (c : char) : Token.t list =
    let cstr = String.of_char c in
    posref := !posref + 1;
    Token.(
      let line = !lineref
      and pos = !posref in
      match c with
      | '(' -> { tt = Left_Paren ; lexeme = cstr; line ; pos } :: acc
      | ')' -> { tt = Right_Paren ; lexeme = cstr; line ; pos } :: acc
      | '{' -> { tt = Left_Brace ; lexeme = cstr; line ; pos } :: acc
      | '}' -> { tt = Right_Brace ; lexeme = cstr; line ; pos } :: acc
      | _ -> acc) in
  let rev_res = Token.{ tt = Eof ; lexeme = "" ; line = !lineref ; pos = !posref + 1 } :: String.fold file_contents ~init:[] ~f in
  List.rev rev_res